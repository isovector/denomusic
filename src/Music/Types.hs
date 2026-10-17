module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Data.Semigroup
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Coerce
import Data.Foldable
import DenoMusic.Types (Reg(..))
import FRP
import GHC.Generics
import Music.Harmony
import Music.Types.Note
import Music.Types.Scale
import Music.Types.Vertical
import Prelude hiding (id, (.))


data Dyad = Lo | Hi
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Triad = Root | Third | Fifth
  deriving stock (Eq, Ord, Show, Enum, Bounded)


divModNote :: forall n. KnownNat n => Note n -> Reg (Note n)
divModNote (Note d as)
  = uncurry Reg
  . fmap (flip Note as)
  $ coerce (flip divMod $ scaleSize @n) d


undivModNote :: forall n. KnownNat n => Reg (Note n) -> Note n
undivModNote (Reg n d) = d + fromIntegral (n * scaleSize @n)


-- | Add a duration for each event that lasts until the next clocked time
-- (regardless of whether that clocked time is 'Event' or 'NoEvent'.)
extending :: SF (Event a) (Event (Timed a))
extending = evStream $ \es ->
  zipWith (\(t1, ea) (t2, _) -> (t1, fmap (Timed $ t2 - t1) ea)) es $ drop 1 es


newtype Horizontal i o = Horizontal
  { unHorizontal :: SF i (o, Event ())
  }
  deriving stock (Generic, Functor)
  deriving (Applicative) via WrappedArrow Horizontal i

instance Category Horizontal where
  id = Horizontal $ id &&& at 0 ()
  Horizontal h2 . Horizontal h1 = Horizontal $ proc a -> do
    (b, e1) <- h1 -< a
    (c, e2) <- h2 -< b
    e' <- latestOf -< e1 <> e2
    returnA -< (c, e')

instance Arrow Horizontal where
  arr f = Horizontal $ arr f &&& at 0 ()
  Horizontal f *** Horizontal g = Horizontal $ proc (b, b') -> do
    (c,  e1) <- f -< b
    (c', e2) <- g -< b'
    e' <- latestOf -< e1 <> e2
    returnA -< ((c, c'), e')

instance Semigroup (Horizontal i o) where
  Horizontal x <> Horizontal y = Horizontal $ switch (x >>> arr (\(o, e) -> ((o, e), e))) $ const y

instance Monoid (Horizontal i (Event o)) where
  mempty = Horizontal $ arr (const NoEvent) &&& at 0 ()


latestOf :: SF (Event a) (Event a)
latestOf = ev2ev $ \case
  [] -> []
  xs -> [last xs]


resize :: Rational -> Horizontal i o -> Horizontal i o
resize dt (Horizontal sf) = Horizontal $ stretch dt sf


replicateH :: Int -> Horizontal i (Event o) -> Horizontal i (Event o)
replicateH = stimes


getHorizontal :: Horizontal i o -> SF i o
getHorizontal = fmap fst . unHorizontal

horizontal :: Horizontal i (Event o) -> SF i (Event (Timed o))
horizontal h = extending . getHorizontal h

rest :: Time -> Horizontal i (Event a)
rest t = Horizontal $ proc i -> do
  es <- at 0 NoEvent -< i
  end <- at t () -< i
  returnA -< (join es, end)

hit :: Time -> a -> Horizontal i (Event a)
hit t = hitWith t . const

dnote :: a -> Horizontal i (Event a)
dnote = hit 1

with :: (i -> a) -> Horizontal i (Event a)
with = hitWith 1

hitWith :: Time -> (i -> a) -> Horizontal i (Event a)
hitWith t f = Horizontal $ proc i -> do
  es <- discrete [(0, Event ()), (t, NoEvent)] -< i
  end <- at t () -< i
  returnA -< (f i <$ join es, end)

