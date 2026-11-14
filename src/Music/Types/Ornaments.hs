{-# LANGUAGE AllowAmbiguousTypes #-}

module Music.Types.Ornaments where

import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntMap.Monoidal (MonoidalIntMap(..))
import Data.Semigroup (Sum(..))
import GHC.Generics (Generic)
import GHC.TypeLits
import Music.Types.Deg


newtype Ornaments a = Ornaments
  { getOrnaments :: IntMap a
  }
  deriving stock (Generic, Functor)
  deriving newtype (Show)
  deriving (Semigroup, Monoid) via MonoidalIntMap (Sum a)

instance (Eq a, Num a) => Eq (Ornaments a) where
  (==) = on (==) (getOrnaments . normalizeOrnaments)

instance (Ord a, Num a) => Ord (Ornaments a) where
  compare = on compare (getOrnaments . normalizeOrnaments)


getOrnament :: forall n. KnownNat n => Ornaments Int -> Deg n
getOrnament = maybe 0 Deg . IM.lookup (scaleSize @n) . getOrnaments

ornament :: forall n a. KnownNat n => a -> Ornaments a
ornament k = Ornaments $ IM.singleton (scaleSize @n) k

normalizeOrnaments :: (Num a, Eq a) => Ornaments a -> Ornaments a
normalizeOrnaments (Ornaments m) = Ornaments $ IM.filter (/= 0) m

