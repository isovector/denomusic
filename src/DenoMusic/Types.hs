module DenoMusic.Types
  ( module DenoMusic.Types
  , Interval (..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Function.Step.Discrete.Open
import Data.Functor.Compose
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map qualified as M
import Data.Maybe hiding (catMaybes)
import GHC.Generics
import Witherable hiding (filter)
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MM


-- | Attach a register to some value.
data Reg a = Reg
  { getReg :: Int
  , unReg :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

instance Applicative Reg where
  pure = Reg 0
  (<*>) = ap

instance Monad Reg where
  Reg r a >>= f = withReg (+ r) $ f a


fromReg :: Reg a -> (a, Int)
fromReg (Reg i a) = (a, i)


withReg :: (Int -> Int) -> Reg a -> Reg a
withReg f (Reg r a) = Reg (f r) a


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
data Music v a = Music
  { duration :: Rational
  , getVoices :: MonoidalMap v (Voice a)
  }
  deriving stock (Functor, Generic, Foldable, Traversable)

instance (Ord v, Semigroup a) => Semigroup (Music v a) where
  Music a1 b1 <> Music a2 b2 = Music (a1 + a2) (b1 <> b2)

instance (Ord v, Monoid a) => Monoid (Music v a) where
  mempty = Music 0 mempty

instance (Ord v, Enum v, Bounded v) => Applicative (Music v) where
  pure a = Music 0 $ MM.fromList $ do
    v <- enumFromTo minBound maxBound
    pure (v, pure a)
  liftA2 f (Music a1 b1) (Music a2 b2)
    = Music (max a1 a2)
    $ MM.mergeWithKey (\_ x y -> Just $ liftA2 f x y)
        (const MM.empty)
        (const MM.empty)
        b1 b2

instance (Enum v, Bounded v, Ord v, Show v, Show a) => Show (Music v a) where
  show = show . toVoices

-- | Replace any 'rest's in the first 'Music' with their counterparts in the
-- second 'Music'.
instance (Ord v, Enum v, Bounded v) => Alternative (Music v) where
  empty = Music 0 MM.empty
  Music d1 m1 <|> Music d2 m2 = Music (max d1 d2) $ MM.unionWith (<|>) m1 m2


-- | When we have a finite number of voices, we can enumerate them into
-- a 'Map'.
toVoices :: (Enum v, Bounded v, Ord v) => Music v a -> M.Map v (Voice a)
toVoices (Music _ m) = M.fromList $ do
  v <- enumFromTo minBound maxBound
  pure (v, fromMaybe emptyVoice $ MM.lookup v m)

emptyVoice :: Voice a
emptyVoice = Voice $ SF M.empty Nothing

instance Filterable (Music v) where
  catMaybes (Music d v) = Music d $ fmap catMaybes v


instance Witherable (Music v)

mapVoices :: Ord v' => (v -> v') -> Music v a -> Music v' a
mapVoices f (Music d m) = Music d $ MM.mapKeys f m

-- instance Bifunctor Music where
--   bimap f g (Music d m) = Music d $ MM.mapWithKey (\a b -> _) m
--   -- lmap f (Music d m) = Music d $ m . f
--   -- rmap = fmap


-- | A 'Voice' is a discretization of the number line into regions of maybe
-- @a@. Absent values correspond to rests, while present values correspond to
-- notes. Attached is a number tracking the size of the "relevant window,"
-- which we use in '(##)' to play one piece of music after another.
newtype Voice a = Voice { unVoice :: SF Rational (Maybe a) }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving newtype (Semigroup, Monoid)
  deriving (Applicative) via Compose (SF Rational) Maybe

instance Filterable Voice where
  catMaybes (Voice (SF m a)) = Voice $ SF (catMaybes m) (catMaybes a)


-- | Replace rests in one 'Voice' with notes from another. The resulting
-- rhythmic pattern will contain boundaries of both arguments.
instance Alternative Voice where
  empty = Voice $ SF mempty Nothing
  Voice s1 <|> Voice s2 = Voice $ liftA2 (<|>) s1 s2


-- | Fold a 'Voice' down into its underlying intervals of sound.
flatten :: Voice a -> [(Interval Rational, a)]
flatten (Voice (SF m _)) = do
  let m' = filter ((>= 0) . fst) $ M.toList m
  ((lo, _), (hi, ma)) <- zip ((0, error "bad") : m') m'
  guard $ lo /= hi
  a <- maybeToList ma
  pure (Interval lo hi, a)


-- | Sample a voice at a given time.
sample :: Voice a -> Rational -> Maybe a
sample (Voice sf) t = sf ! t
