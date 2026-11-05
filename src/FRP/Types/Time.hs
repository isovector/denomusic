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


data Span a = Span
  { duration :: Time
  , getSpan :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Sequential composition; takes the sum of the two durations.
instance Semigroup a => Semigroup (Span a) where
  Span t1 a <> Span t2 b = Span (t1 + t2) (a <> b)

instance Monoid a => Monoid (Span a) where
  mempty = Span 0 mempty

-- | Parallel composition; takes the max of the two durations.
instance Applicative Span where
  pure = Span 0
  liftA2 f (Span ta a) (Span tb b) = Span (max ta tb) $ f a b
