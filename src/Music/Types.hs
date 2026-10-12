module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Control.Applicative
import Control.Arrow
import Control.Arrow.Transformer.Static
import Control.Category
import Control.Lens hiding (at)
import Data.Coerce
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

class HasDuration a where
  duration :: Traversal' a Time

instance HasDuration Time where
  duration = id

instance HasDuration (Timed t) where
  duration = #tduration

instance HasDuration (Horizontal i o) where
  duration = field' @"unHorizontal" . duration

instance HasDuration a => HasDuration (Event a) where
  duration = #_MkEvent . _Just . duration

timed :: HasDuration a => a -> Horizontal t (Event a)
timed = Horizontal . uncurry Timed . (view duration &&& at 0)

newtype Horizontal i o = Horizontal
  { unHorizontal :: Timed (SF i o)
  }
  deriving stock (Generic, Functor)
  deriving (Applicative) via WrappedArrow (StaticArrow Timed SF) i
  deriving (Category, Arrow, ArrowLoop) via StaticArrow Timed SF


instance Semigroup (Horizontal i o) where
  Horizontal (Timed t1 x) <> Horizontal (Timed t2 y) =
    Horizontal $ Timed (t1 + t2) $ switch (x &&& at t1 ()) $ const y

instance Monoid o => Monoid (Horizontal i o) where
  mempty = Horizontal $ pure mempty


resize :: HasDuration o => Rational -> Horizontal i o -> Horizontal i o
resize d (Horizontal (Timed t sf)) =
  Horizontal $ Timed (fromRational d * t) $ fmap (duration *~ fromRational d) $ sf >>> stretch d

getHorizontal :: Horizontal i o -> SF i o
getHorizontal = unTimed . unHorizontal

rest :: Time -> Horizontal i (Event a)
rest t = Horizontal $ Timed t $ pure NoEvent

hit :: Time -> a -> Horizontal a (Event (Timed a))
hit t a = timed $ Timed t a

