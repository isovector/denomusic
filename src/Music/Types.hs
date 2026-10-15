module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens hiding (at)
import Data.Coerce
import Data.Foldable
import Data.Generics.Labels ()
import Data.Generics.Product (field')
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

data Timed a = Timed
  { tduration :: Time
  , unTimed :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Sequential composition; takes the sum of the two durations.
instance Semigroup a => Semigroup (Timed a) where
  Timed t1 a <> Timed t2 b = Timed (t1 + t2) (a <> b)

instance Monoid a => Monoid (Timed a) where
  mempty = Timed 0 mempty

-- | Parallel composition; takes the max of the two durations.
instance Applicative Timed where
  pure = Timed 0
  liftA2 f (Timed ta a) (Timed tb b) = Timed (max ta tb) $ f a b


instance HasDuration (Timed t) where
  duration = #tduration

instance HasDuration (Horizontal i o) where
  duration = field' @"unHorizontal" . duration


timed :: HasDuration a => a -> Horizontal t a
timed a = hit (view duration a) a

newtype Horizontal i o = Horizontal
  { unHorizontal :: Timed (SF i (Event (Timed o)))
  }
  deriving stock (Generic, Functor)
  deriving (Applicative) via WrappedArrow Horizontal i

instance Category Horizontal where
  id = Horizontal $ pure $ arr $ Event . pure
  Horizontal (Timed t2 g) . Horizontal (Timed t1 f)
    = Horizontal
    $ Timed (t1 + t2)
    $ hoistSF g . f



-- TODO(sandy): Is this safe? unclear but plausible
hoistSF :: SF i (Event (Timed o)) -> SF (Event (Timed i)) (Event (Timed o))
hoistSF sf = proc ei -> do
  eo <- sf -< event undefined unTimed ei
  returnA -< liftA2 (liftA2 $ flip const) ei eo

instance Arrow Horizontal where
  arr f = Horizontal $ pure $ arr (Event . pure . f)
  Horizontal (Timed t1 f) *** Horizontal (Timed t2 g)
    = Horizontal
    $ Timed (max t1 t2)
    $ proc (b, b') -> do
        ec  <- f -< b
        ec' <- g -< b'
        returnA -< liftA2 (liftA2 (,)) ec ec'


replicateH :: Int -> Horizontal i o -> Horizontal i o
replicateH n = fold . replicate n


instance Semigroup (Horizontal i o) where
  Horizontal (Timed t1 x) <> Horizontal (Timed t2 y) =
    Horizontal $ Timed (t1 + t2) $ switch (x &&& at t1 ()) $ const y

instance Monoid (Horizontal i o) where
  mempty = Horizontal $ pure $ arr $ const NoEvent


resize :: Rational -> Horizontal i o -> Horizontal i o
resize d (Horizontal (Timed t sf)) =
  Horizontal $ Timed (fromRational d * t) $ fmap (duration *~ fromRational d) $ stretch d sf


getHorizontal :: Horizontal i o -> SF i (Event (Timed o))
getHorizontal = unTimed . unHorizontal

rest :: Time -> Horizontal i a
rest t = Horizontal $ Timed t $ pure NoEvent

hit :: Time -> a -> Horizontal i a
hit t = hitWith t . const

dnote :: a -> Horizontal i a
dnote = hit 1

with :: (i -> a) -> Horizontal i a
with = hitWith 1

hitWith :: Time -> (i -> a) -> Horizontal i a
hitWith t f = Horizontal $ Timed t $ fmap (fmap $ Timed t . f) $ at' 0

