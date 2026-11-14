{-# LANGUAGE AllowAmbiguousTypes #-}

module Music.Types.Deg where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits


type Deg :: Nat -> Type
newtype Deg n = Deg
  { getDeg :: Int
  }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)


scaleSize :: forall n a. (KnownNat n, Num a) => a
scaleSize = fromInteger $ natVal $ Proxy @n

