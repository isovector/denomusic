{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Types.SF where

import Control.Applicative (WrappedArrow(..))
import Control.Arrow
import Control.Category
import Data.Functor
import Data.MemoTrie
import Data.Monoid
import Data.Ratio
import FRP.Types.Time
import FRP.Types.Signal
import Prelude hiding (id, (.))


newtype SF a b = SF
  { runSF :: Signal a -> Signal b
  }
  deriving (Functor, Applicative) via WrappedArrow SF a
  deriving (Semigroup, Monoid) via Ap (SF a) b

instance Category SF where
  id = SF id
  SF g . SF f = SF (g . f)

instance Arrow SF where
  arr = SF . fmap
  SF f *** SF g = SF $ \sg ->
    liftA2 (,) (f $ fmap fst sg) (g $ fmap snd sg)

instance ArrowLoop SF where
  loop (SF f) = SF $ \a -> do
    let b = Signal (memo $ \t -> snd $ sample out t) []
        out = f $ liftA2 (,) a b
    fmap fst out

instance ArrowApply SF where
  app = SF $ \s -> do
    let sf' = fmap fst s
        sb = fmap snd s
        cc = fmap (flip runSF sb) sf'
    Signal (memo $ \t -> sample (sample cc t) t) []



sample :: Signal a -> Time -> a
sample (Signal fa []) t = fa t
sample (Signal fa ((t0, sa) : as)) t =
  case compare t t0 of
    LT -> fa t
    EQ ->
      case sa of
        Step Nothing f -> f t
        Step (Just a) _ -> a
    GT -> sample (Signal (step sa) as) t


instance HasTrie Rational where
  data Rational :->: x = RationalTrie (Integer :->: (Integer :->: x))
  trie f = RationalTrie $ trie $ trie . \n d -> f (n % d)
  untrie (RationalTrie x) = (\f r -> f (numerator r) (denominator r)) (untrie . untrie x)
  enumerate = error "no enumerate for Rational"

