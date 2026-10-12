module Music.Types.Vertical
  ( module Music.Types.Vertical
  , Distributive(..)
  ) where

import Control.DeepSeq
import Control.Lens
import Data.Distributive
import Data.Foldable
import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(..))
import FRP
import GHC.Generics


-- | A vertical ("simultaneous") configuration of some sort.
newtype Vertical v a = Vertical
  { getVertical :: v -> a
  }
  deriving stock (Functor, Generic)
  deriving newtype (Applicative, Monad, Profunctor)
  deriving (Num, Semigroup, Monoid) via Ap (Vertical v) a

prettyVertical :: (Enum v, Bounded v, Show v) => Vertical v String -> String
prettyVertical
  = flip mappend " }"
  . mappend "{ "
  . intercalate ", "
  . fmap (\(v, a) -> mconcat [show v, ": ", a])
  . enumerate


voice :: Eq v => v -> Lens' (Vertical v a) a
voice v = lens (($ v ) . getVertical) (\(Vertical f) a -> Vertical $ \case
  v' | v == v' -> a
     | otherwise -> f v')


iota :: (Enum v, Num a) => Vertical v a
iota = Vertical $ \i ->
  fromIntegral $ fromEnum i


instance Distributive (Vertical v) where
  distribute f = Vertical $ \v -> fmap (($ v) . getVertical) f

instance NFData (Vertical v a)

instance (Enum v, Bounded v, Eq a) => Eq (Vertical v a) where
  (==) = on (==) toList

instance (Enum v, Bounded v, Ord a) => Ord (Vertical v a) where
  compare = on compare toList

instance (Enum v, Bounded v, Show v, Show a) => Show (Vertical v a) where
  showsPrec p = showsPrec p . enumerate


enumerate :: (Enum v, Bounded v) => Vertical v a -> [(v, a)]
enumerate (Vertical c) = fmap (id &&& c) $ enumFromTo minBound maxBound

unenumerate :: Eq v => [(v, a)] -> Vertical v a
unenumerate = Vertical . fmap (fromMaybe $ error "traverse @Vertical: impossible") . flip lookup


instance (Enum v, Bounded v) => Foldable (Vertical v) where
  foldMap f = foldMap (f . snd) . enumerate

instance (Eq v, Enum v, Bounded v) => Traversable (Vertical v) where
  traverse f
    = fmap unenumerate
    . traverse (traverse f)
    . enumerate


embed :: (v' -> (v, a -> b)) -> Vertical v a -> Vertical v' b
embed f' (Vertical f) = Vertical $ \v' ->
  let (v, fab) = f' v'
   in fab $ f v

