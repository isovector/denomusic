module FRP.Types.Time where

import Data.Ratio
import Data.MemoTrie

newtype Time = Time
  { unTime :: Rational
  }
  deriving newtype (Eq, Ord, Show, Enum, Num, Real, Fractional)


instance HasTrie Time where
  data Time :->: x = RationalTrie (Integer :->: (Integer :->: x))
  trie f = RationalTrie $ trie $ trie . \n d -> f (Time $ n % d)
  untrie (RationalTrie x) = (\f (Time r) -> f (numerator r) (denominator r)) (untrie . untrie x)
  enumerate = error "no enumerate for Time"


instance Semigroup Time where
  (<>) = max

instance Monoid Time where
  mempty = 0

