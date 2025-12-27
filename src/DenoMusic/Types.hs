module DenoMusic.Types
  ( module DenoMusic.Types
  , Interval (..)
  , Reg (..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Function.Step.Discrete.Open
import Data.Functor.Compose
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map qualified as M
import Data.Maybe
import Music.Harmony
import Data.Monoid
import Data.Profunctor
import GHC.Generics


data PitchClass
  =      C | Cs
  | Df | D | Ds
  | Ef | E
  |      F | Fs
  | Gf | G | Gs
  | Af | A | As
  | Bf | B
  deriving stock (Show, Eq, Ord, Read, Enum, Bounded)

-- | Music is a mapping of voice labels to 'Voice's.
newtype Music v a = Music
  { getVoices :: v -> Voice a
  }
  deriving stock (Functor, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving Applicative via (Compose ((->) v) Voice)

instance (Enum v, Bounded v, Ord v, Show v, Show a) => Show (Music v a) where
  show = show . toVoices

-- | Replace any 'rest's in the first 'Music' with their counterparts in the
-- second 'Music'.
instance Alternative (Music v) where
  empty = Music $ const empty
  Music m1 <|> Music m2 = Music $ \v -> m1 v <|> m2 v

instance (Enum v, Bounded v) => Foldable (Music v) where
  foldMap f (Music m) =
    foldMap (\v -> foldMap f $ m v) $ enumFromTo minBound maxBound


-- | When we have a finite number of voices, we can enumerate them into
-- a 'Map'.
toVoices :: (Enum v, Bounded v, Ord v) => Music v a -> M.Map v (Voice a)
toVoices (Music m) = M.fromList $ do
  v <- enumFromTo minBound maxBound
  pure (v, m v)

instance (Enum v, Bounded v, Ord v) => Traversable (Music v) where
  traverse f m
    = fmap (\m' -> Music $ \v -> m' M.! v)
    $ traverse (traverse f)
    $ toVoices m

-- | 'Music' forms a profunctor on its voice labels; 'lmap' corresponds to
-- trading lines between voices.
instance Profunctor Music where
  lmap f (Music m) = Music $ m . f
  rmap = fmap


-- | A 'Voice' is a discretization of the number line into regions of maybe
-- @a@. Absent values correspond to rests, while present values correspond to
-- notes. Attached is a number tracking the size of the "relevant window,"
-- which we use in '(##)' to play one piece of music after another.
data Voice a
  = Voice (Sum Rational) (SF Rational (Maybe a))
  | Drone a
  | Empty
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Pointwise semigroup multiplication. Combine the notes being played
-- simultaneously, and subdivide notes whenever necessary to align the
-- boundaries.
--
-- Unlike the 'Applicative' instance, this treats 'rest' as 'mempty'.
instance Semigroup a => Semigroup (Voice a) where
  Empty <> v = v
  v <> Empty = v
  Drone a <> Drone b = Drone (a <> b)
  Drone a <> Voice d s = Voice d (fmap (Just a <>) s)
  Voice d s <> Drone b = Voice d (fmap (<> Just b) s)
  Voice d1 s1 <> Voice d2 s2 = Voice (d1 <> d2) (s1 <> s2)

instance Semigroup a => Monoid (Voice a) where
  mempty = Empty

-- | Pointwise application of 'Voice's.
--
-- Be careful with this instance, since 'rest's in either argument will
-- propagate through.
instance Applicative Voice where
  pure = Drone
  liftA2 _ Empty _ = Empty
  liftA2 _ _ Empty = Empty
  liftA2 f (Drone a) (Drone b) = Drone $ f a b
  liftA2 f (Voice d1 a) (Drone b) = Voice d1 $ fmap (fmap $ flip f b) a
  liftA2 f (Drone a) (Voice d2 b) = Voice d2 $ fmap (fmap $ f a) b
  liftA2 f (Voice d1 a) (Voice d2 b) =
    Voice (d1 <> d2) $ liftA2 (liftA2 f) a b

-- | Replace rests in one 'Voice' with notes from another. The resulting
-- rhythmic pattern will contain boundaries of both arguments.
instance Alternative Voice where
  empty = Empty
  Empty <|> v = v
  v <|> Empty = v
  Drone a <|> _ = Drone a
  Voice d s <|> Drone a = Voice d $ fmap (<|> Just a) s
  Voice d1 s1 <|> Voice d2 s2 = Voice (d1 <> d2) $ liftA2 (<|>) s1 s2


-- | Fold a 'Voice' down into its underlying intervals of sound.
flatten :: Voice a -> [(Interval Rational, a)]
flatten (Voice _ (SF m _)) = do
  let m' = filter ((>= 0) . fst) $ M.toList m
  ((lo, _), (hi, ma)) <- zip ((0, error "bad") : m') m'
  guard $ lo /= hi
  a <- maybeToList ma
  pure (Interval lo hi, a)
flatten Drone {} = mempty
flatten Empty = mempty


-- | Sample a voice at a given time.
sample :: Voice a -> Rational -> Maybe a
sample (Voice _ sf) t = sf ! t
sample (Drone a) _ = Just a
sample Empty _ = Nothing
