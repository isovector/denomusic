module Music2
  ( module Music2
  , Interval(..)
  ) where

import Data.Map.Monoidal qualified as MM
import Data.Map.Monoidal (MonoidalMap)
import Data.Monoid
import GHC.Generics
import Control.Monad
import Data.Function.Step.Discrete.Open
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map qualified as M

data Music v a
  = Everywhere a
  | Voices (MonoidalMap v (Voice a))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Ord v => Applicative (Music v) where
  pure = Everywhere
  liftA2 f (Everywhere a) (Everywhere b) = Everywhere $ f a b
  liftA2 f (Voices a) (Everywhere b) = Voices $ fmap (fmap $ flip f b) a
  liftA2 f (Everywhere a) (Voices b) = Voices $ fmap (fmap $ f a) b
  liftA2 f (Voices a) (Voices b) =
    Voices $
      MM.mergeWithKey
        (const $ fmap pure . liftA2 f)
        (const MM.empty)
        (const MM.empty)
        a
        b

instance (Ord v, Semigroup a) => Semigroup (Music v a) where
  Everywhere a <> Everywhere b = Everywhere (a <> b)
  Voices a <> Everywhere b = Voices $ fmap (fmap (<> b)) a
  Everywhere a <> Voices b = Voices $ fmap (fmap (a <>)) b
  Voices a <> Voices b = Voices $ a <> b

instance (Ord v, Monoid a) => Monoid (Music v a) where
  mempty = Everywhere mempty


data Voice a
  = Voice (Sum Rational) (SF Rational a)
  | Always a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Voice a

instance Applicative Voice where
  pure = Always
  liftA2 f (Always a) (Always b) = Always $ f a b
  liftA2 f (Voice d1 a) (Always b) = Voice d1 $ fmap (flip f b) a
  liftA2 f (Always a) (Voice d2 b) = Voice d2 $ fmap (f a) b
  liftA2 f (Voice d1 a) (Voice d2 b) =
    Voice (d1 <> d2) $ liftA2 f a b


delay :: Rational -> Voice a -> Voice a
delay o (Voice d (SF m e)) = Voice d $ SF (M.mapKeys (+ o) m) e
delay _ (Always a) = Always a


-- | The identity on '(##)'.
silent :: Monoid a => Voice a
silent = rest 0


-- | Tile product (eg "play this before that")
(##) :: Monoid a => Voice a -> Voice a -> Voice a
(##) v1@(Voice d _) = liftA2 (<>) v1 . delay (getSum d)
(##) (Always a) = fmap (a <>)
infixr 6 ##


line :: (Foldable t, Monoid a) => t (Voice a) -> Voice a
line = foldr (##) silent


region
  :: Monoid a
  => Rational
  -- ^ Start time
  -> Rational
  -- ^ Stop time
  -> a
  -> Voice a
region lo hi a = Voice (pure $ hi - lo) $ SF (M.fromList [(lo, mempty), (hi, a)]) mempty


note :: Monoid a => Rational -> a -> Voice a
note = region 0


rest :: Monoid a => Rational -> Voice a
rest d = Voice (pure d) mempty


flatten :: Voice a -> [(Interval Rational, a)]
flatten (Voice _ (SF m _)) = do
  let m' = filter ((>= 0) . fst) $ M.toList m
  ((lo, _), (hi, ma)) <- zip ((0, error "impossible") : m') m'
  guard $ lo /= hi
  pure (Interval lo hi, ma)
flatten Always {} = mempty


sample :: Rational -> Voice a -> a
sample t (Voice _ sf) = sf ! t
sample _ (Always a) = a

