module FRP.Types.Time where

import GHC.Generics
import Data.Ratio
import Data.MemoTrie

newtype Time = Time
  { unTime :: Rational
  }
  deriving newtype (Eq, Ord, Show, Enum, Num, Real, Fractional)
  deriving stock Generic


instance HasTrie Time where
  data Time :->: x = RationalTrie (Integer :->: (Integer :->: x))
  trie f = RationalTrie $ trie $ trie . \n d -> f (Time $ n % d)
  untrie (RationalTrie x) = (\f (Time r) -> f (numerator r) (denominator r)) (untrie . untrie x)
  enumerate = error "no enumerate for Time"


instance Semigroup Time where
  (<>) = max

instance Monoid Time where
  mempty = 0


data Timed a = Timed
  { duration :: Time
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
