module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Control.Monad
import Control.Arrow.Transformer.Static
import Control.Applicative
import Control.Arrow
import Control.Category
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
  { unHorizontal :: Timed (SF i o)
  }
  deriving stock (Generic, Functor)
  deriving (Category, Arrow, ArrowLoop) via StaticArrow Timed SF
  deriving (Applicative) via WrappedArrow Horizontal i

resize :: Rational -> Horizontal i o -> Horizontal i o
resize dt (Horizontal (Timed d sf)) = Horizontal $ Timed (fromRational dt * d) $ stretch dt sf


replicateH :: Int -> Horizontal i (Event o) -> Horizontal i (Event o)
replicateH n = fold . replicate n


holdH :: o -> Horizontal (Event o) o
holdH = Horizontal . pure . hold


instance Semigroup (Horizontal i o) where
  Horizontal (Timed t1 x) <> Horizontal (Timed t2 y) =
    Horizontal $ Timed (t1 + t2) $ switch (x &&& at t1 ()) $ const y

instance Monoid (Horizontal i (Event o)) where
  mempty = Horizontal $ pure $ arr $ const NoEvent


getHorizontal :: Horizontal i o -> SF i o
getHorizontal = unTimed . unHorizontal

horizontal :: Horizontal i (Event o) -> SF i (Event (Timed o))
horizontal h = extending . unTimed (unHorizontal h)

rest :: Time -> Horizontal i (Event a)
rest t = Horizontal $ Timed t $ proc i -> do
  es <- at 0 NoEvent -< i
  returnA -< join es

hit :: Time -> a -> Horizontal i (Event a)
hit t = hitWith t . const

dnote :: a -> Horizontal i (Event a)
dnote = hit 1

with :: (i -> a) -> Horizontal i (Event a)
with = hitWith 1

hitWith :: Time -> (i -> a) -> Horizontal i (Event a)
hitWith t f = Horizontal $ Timed t $ proc i -> do
  es <- discrete [(0, Event ()), (t, NoEvent)] -< i
  returnA -< f i <$ join es

