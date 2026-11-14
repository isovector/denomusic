{-# LANGUAGE AllowAmbiguousTypes #-}

module Music.Types.Ornaments where

import Data.Coerce
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


decompOrnament :: forall n. KnownNat n => Ornaments Int -> (Ornaments Int, Deg n)
decompOrnament o
  = maybe
      (o, 0)
      ( (coerce (IM.filterWithKey @Int (\k _ -> k /= scaleSize @n)) o, )
      . Deg
      )
  $ IM.lookup (scaleSize @n)
  $ getOrnaments o


getOrnament :: forall n. KnownNat n => Ornaments Int -> Deg n
getOrnament = snd . decompOrnament

ornament :: forall n a. KnownNat n => a -> Ornaments a
ornament k = Ornaments $ IM.singleton (scaleSize @n) k

normalizeOrnaments :: (Num a, Eq a) => Ornaments a -> Ornaments a
normalizeOrnaments (Ornaments m) = Ornaments $ IM.filter (/= 0) m

