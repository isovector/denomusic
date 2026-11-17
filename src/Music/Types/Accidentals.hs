{-# LANGUAGE AllowAmbiguousTypes #-}

module Music.Types.Accidentals where

import Data.Coerce
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntMap.Monoidal (MonoidalIntMap(..))
import Data.Semigroup (Sum(..))
import GHC.Generics (Generic)
import GHC.TypeLits
import Music.Types.Deg


newtype Accidentals a = Accidentals
  { getAccidentals :: IntMap a
  }
  deriving stock (Generic, Functor)
  deriving newtype (Show)
  deriving (Semigroup, Monoid) via MonoidalIntMap (Sum a)

normalizeAccidentals :: (Num a, Eq a) => Accidentals a -> Accidentals a
normalizeAccidentals = Accidentals . IM.filter (/= 0) . getAccidentals

instance (Eq a, Num a) => Eq (Accidentals a) where
  (==) = on (==) (getAccidentals . normalizeAccidentals)

instance (Ord a, Num a) => Ord (Accidentals a) where
  compare = on compare (getAccidentals . normalizeAccidentals)


decompAccidental :: forall n. KnownNat n => Accidentals Int -> (Accidentals Int, Deg n)
decompAccidental o
  = maybe
      (o, 0)
      ( (coerce (IM.filterWithKey @Int (\k _ -> k /= scaleSize @n)) o, )
      . Deg
      )
  $ IM.lookup (scaleSize @n)
  $ getAccidentals o


getAccidental :: forall n. KnownNat n => Accidentals Int -> Deg n
getAccidental = snd . decompAccidental

accidental :: forall n a. KnownNat n => a -> Accidentals a
accidental = Accidentals . IM.singleton (scaleSize @n)

