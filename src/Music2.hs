module Music2
  ( module Music2
  , module Data.Functor.Apply
  , Interval(..)
  ) where

import Data.Functor.Apply
import Data.Monoid
import Data.Functor.Compose
import GHC.Generics
import Control.Monad
import Data.Function.Step.Discrete.Open
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map qualified as M


data Voice a
  = Voice
    { v_duration :: Bounds Rational
    , v_voice :: SF Rational a
    }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Generically (Voice a)

instance Apply Voice where
  liftF2 f (Voice d1 sf1) (Voice d2 sf2) =
    Voice (liftA2 (+) d1 d2) $ liftA2 f sf1 sf2

newtype Bounds a = Bounds
  { unBounded :: Maybe a
  }
  deriving stock (Foldable, Traversable)
  deriving newtype (Eq, Ord, Show, Functor, Applicative)
  deriving (Semigroup, Monoid) via Compose Maybe Sum a


delay :: Rational -> Voice a -> Voice a
delay o (Voice d (SF m e)) = Voice d $ SF (M.mapKeys (+ o) m) e
-- delay o (LiftA2 f x y) = LiftA2 f (delay o x) (delay o y)


-- | The identity on 'after'.
silent :: Monoid a => Voice a
silent = rest 0


-- | Tile product (eg "play this before that")
(##) :: Monoid a => Voice a -> Voice a -> Voice a
(##) v1@(Voice d _) = liftF2 (<>) v1 . maybe id delay (unBounded d)
infixr 6 ##


line :: (Foldable t, Monoid a) => t (Voice a) -> Voice a
line = foldr (##) silent


always :: a -> Voice a
always = Voice (Bounds Nothing) . constant


bounded :: Monoid a => Rational -> Rational -> a -> Voice a
bounded lo hi a = Voice (pure $ hi - lo) $ SF (M.fromList [(lo, mempty), (hi, a)]) mempty


note :: Monoid a => Rational -> a -> Voice a
note = bounded 0


rest :: Monoid a => Rational -> Voice a
rest d = Voice (pure d) mempty


flatten :: Voice a -> [(Interval Rational, a)]
flatten (Voice _ (SF m _)) = do
  let m' = filter ((>= 0) . fst) $ M.toList m
  ((lo, _), (hi, ma)) <- zip ((0, error "impossible") : m') m'
  guard $ lo /= hi
  pure (Interval lo hi, ma)

