module Music.Types.Vertical where

import Control.DeepSeq
import Data.Foldable
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(..))
import Data.Profunctor
import FRP
import GHC.Generics


-- | A vertical ("simultaneous") configuration of some sort.
newtype Vertical v a = Vertical
  { getVertical :: v -> a
  }
  deriving stock (Functor, Generic)
  deriving newtype (Applicative, Monad, Profunctor)
  deriving (Num, Semigroup, Monoid) via Ap (Vertical v) a

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

