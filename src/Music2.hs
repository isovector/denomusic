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

data Polyphony v a
  = Pure a
  | Polyphonic (MonoidalMap v (Voice a))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Ord v => Applicative (Polyphony v) where
  pure = Pure
  liftA2 f (Pure a) (Pure b) = Pure $ f a b
  liftA2 f (Polyphonic a) (Pure b) = Polyphonic $ fmap (fmap $ flip f b) a
  liftA2 f (Pure a) (Polyphonic b) = Polyphonic $ fmap (fmap $ f a) b
  liftA2 f (Polyphonic a) (Polyphonic b) =
    Polyphonic $
      MM.mergeWithKey
        (const $ fmap pure . liftA2 f)
        (const MM.empty)
        (const MM.empty)
        a
        b

instance (Ord v, Semigroup a) => Semigroup (Polyphony v a) where
  Pure a <> Pure b = Pure (a <> b)
  Polyphonic a <> Pure b = Polyphonic $ fmap (fmap (<> b)) a
  Pure a <> Polyphonic b = Polyphonic $ fmap (fmap (a <>)) b
  Polyphonic a <> Polyphonic b = Polyphonic $ a <> b

instance (Ord v, Monoid a) => Monoid (Polyphony v a) where
  mempty = Pure mempty


data Voice a
  = Voice (Sum Rational) (SF Rational a)
  | Drone a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Voice a

instance Applicative Voice where
  pure = Drone
  liftA2 f (Drone a) (Drone b) = Drone $ f a b
  liftA2 f (Voice d1 a) (Drone b) = Voice d1 $ fmap (flip f b) a
  liftA2 f (Drone a) (Voice d2 b) = Voice d2 $ fmap (f a) b
  liftA2 f (Voice d1 a) (Voice d2 b) =
    Voice (d1 <> d2) $ liftA2 f a b


delay :: Rational -> Voice a -> Voice a
delay o (Voice d (SF m e)) = Voice d $ SF (M.mapKeys (+ o) m) e
delay _ (Drone a) = Drone a


-- | The identity on '(##)'.
silent :: Monoid a => Voice a
silent = rest 0


-- | Tile product (eg "play this before that")
(##) :: Monoid a => Voice a -> Voice a -> Voice a
(##) v1@(Voice d _) = liftA2 (<>) v1 . delay (getSum d)
(##) (Drone a) = fmap (a <>)
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
flatten Drone {} = mempty


sample :: Rational -> Voice a -> a
sample t (Voice _ sf) = sf ! t
sample _ (Drone a) = a

