module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Coerce
import Data.Maybe (catMaybes)
import Data.Semigroup
import DenoMusic.Types (Reg(..))
import FRP
import GHC.Generics
import Music.Harmony
import Music.Types.Note
import Music.Types.Scale
import Music.Types.Vertical
import Prelude hiding (id, (.))


divModNote :: forall n. KnownNat n => Note n -> Reg (Note n)
divModNote (Note d as)
  = uncurry Reg
  . fmap (flip Note as)
  $ coerce (flip divMod $ scaleSize @n @Int) d


undivModNote :: forall n. KnownNat n => Reg (Note n) -> Note n
undivModNote (Reg n d) = d + fromIntegral (n * scaleSize @n)


-- | Add a duration for each event that lasts until the next clocked time
-- (regardless of whether that clocked time is 'Event' or 'NoEvent'.)
extending :: SF (Event (), Event a) (Event (Timed a))
extending = proc (end, a) ->
  ev2ev
    (\es -> catMaybes
      $ zipWith
          (\(t1, ema) (t2, _) -> case ema of
            Just a -> Just (t1, Timed (t2 - t1) a)
            Nothing -> Nothing
          )
          es
      $ drop 1 es
    ) -< fmap Just a <|> Nothing <$ end


data HState o = HState
  { h_out :: o
  , h_evs :: Event ()
    -- ^ All start or stop events.
  , h_end :: Event ()
    -- ^ The end of the Horizontal
  }
  deriving stock (Eq, Ord, Show, Functor)

newtype Horizontal i o = Horizontal
  { unHorizontal :: SF i (HState o)
  }
  deriving stock (Generic, Functor)
  deriving (Applicative) via WrappedArrow Horizontal i

instance Category Horizontal where
  id = Horizontal $ proc a -> do
    e <- now () -< a
    returnA -< HState a NoEvent e
  Horizontal h2 . Horizontal h1 = Horizontal $ proc a -> do
    (HState b es1 e1) <- h1 -< a
    (HState c es2 e2) <- h2 -< b
    e' <- latestOf -< e1 <> e2
    returnA -< (HState c (es1 <> es2) e')

instance Arrow Horizontal where
  arr f = Horizontal $ proc a -> do
    e <- now () -< a
    returnA -< HState (f a) NoEvent e
  Horizontal f *** Horizontal g = Horizontal $ proc (b, b') -> do
    (HState c  es1 e1) <- f -< b
    (HState c' es2 e2) <- g -< b'
    e' <- latestOf -< e1 <> e2
    returnA -< (HState (c, c') (es1 <> es2) e')

instance Semigroup (Horizontal i o) where
  Horizontal x <> Horizontal y = Horizontal $
    switch
      (x >>> arr (\(HState o es e) -> ((HState o es e), e)))
      $ const y

instance Monoid (Horizontal i (Event o)) where
  mempty = Horizontal $ proc a -> do
    e <- now () -< a
    returnA -< HState NoEvent e e


latestOf :: SF (Event a) (Event a)
latestOf = ev2ev $ \case
  [] -> []
  xs -> [last xs]



resize :: Rational -> Horizontal i o -> Horizontal i o
resize dt (Horizontal sf) = Horizontal $ stretch dt sf


replicateH :: Int -> Horizontal i (Event o) -> Horizontal i (Event o)
replicateH = stimes


holdH :: a -> Horizontal (Event a) a
holdH a0 = Horizontal $ proc i -> do
  a <- hold a0 -< i
  returnA -< HState a NoEvent NoEvent

accumH :: a -> Horizontal (Event (a -> a)) (Event a)
accumH a0 = Horizontal $ proc i -> do
  a <- accum a0 -< i
  returnA -< HState a NoEvent NoEvent


getHorizontal :: Horizontal i o -> SF i o
getHorizontal = fmap h_out . unHorizontal

horizontal :: Horizontal i (Event o) -> SF i (Event (Timed o))
horizontal h = extending <<< fmap (h_evs &&& h_out) (unHorizontal h)

rest :: Time -> Horizontal i (Event a)
rest t = Horizontal $ proc i -> do
  end <- at t () -< i
  returnA -< HState NoEvent end end

hit :: Time -> a -> Horizontal i (Event a)
hit t = hitWith t . const

dnote :: a -> Horizontal i (Event a)
dnote = hit 1

with :: (i -> a) -> Horizontal i (Event a)
with = hitWith 1

hitWith :: Time -> (i -> a) -> Horizontal i (Event a)
hitWith t f = Horizontal $ proc i -> do
  e0 <- now () -< i
  end <- at t () -< i
  returnA -< HState (f i <$ e0) (e0 <> end) end

hitDyn :: Horizontal (Time, a) (Event a)
hitDyn = Horizontal $ proc i -> do
  (a, end) <- dynamicE -< i
  e <- now () -< i
  returnA -< HState (a <$ e) (e <> end) end

