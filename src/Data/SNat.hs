{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd
                  2022-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStarIsType #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Data.SNat
  ( -- * Singleton natural numbers
    -- ** Data type
    SNat (..)
    -- ** Construction
  , snatProxy
  , withSNat
    -- ** Conversion
  , snatToInteger, snatToNatural, snatToNum
    -- ** Conversion (ambiguous types)
  , natToInteger, natToNatural, natToNum
    -- ** Arithmetic
  , addSNat, mulSNat, powSNat, succSNat
    -- *** Partial
  , subSNat, divSNat, modSNat, predSNat
    -- *** Specialised
  , pow2SNat
    -- *** Comparison
  , SNatLE (..), compareSNat
    -- * Unary/Peano-encoded natural numbers
    -- ** Data type
  , UNat (..)
    -- ** Construction
  , toUNat
    -- ** Conversion
  , fromUNat
    -- ** Arithmetic
  , addUNat, mulUNat, powUNat
    -- *** Partial
  , predUNat, subUNat
  )
where

import Data.Kind          (Type)
import Data.Type.Equality ((:~:)(..))
import GHC.Show           (appPrec)
import GHC.TypeLits hiding (SNat)
import GHC.Natural        (naturalFromInteger)

{- $setup
>>> :set -XBinaryLiterals
>>> import Clash.Promoted.Nat.Literals (d789)
-}

-- | Singleton value for a type-level natural number @n@
--
-- * "Clash.Promoted.Nat.Literals" contains a list of predefined 'SNat' literals
-- * "Clash.Promoted.Nat.TH" has functions to easily create large ranges of new
--   'SNat' literals
data SNat (n :: Nat) where
  SNat :: KnownNat n => SNat n

-- | Create an @`SNat` n@ from a proxy for /n/
snatProxy :: KnownNat n => proxy n -> SNat n
snatProxy _ = SNat

instance Show (SNat n) where
  showsPrec d p@SNat | n <= 1024 = showChar 'd' . shows n
                     | otherwise = showParen (d > appPrec) $
                                     showString "SNat @" . shows n
   where
    n = snatToInteger p

{-# INLINE withSNat #-}
-- | Supply a function with a singleton natural @n@ according to the context
withSNat :: KnownNat n => (SNat n -> a) -> a
withSNat f = f SNat

-- | Same as 'snatToInteger' and 'GHC.TypeLits.natVal', but doesn't take term
-- arguments. Example usage:
--
-- >>> natToInteger @5
-- 5
natToInteger :: forall n . KnownNat n => Integer
natToInteger = snatToInteger (SNat @n)
{-# INLINE natToInteger #-}

-- | Reify the type-level 'Nat' @n@ to it's term-level 'Integer' representation.
snatToInteger :: SNat n -> Integer
snatToInteger p@SNat = natVal p
{-# INLINE snatToInteger #-}

-- | Same as 'snatToNatural' and 'GHC.TypeNats.natVal', but doesn't take term
-- arguments. Example usage:
--
-- >>> natToNatural @5
-- 5
natToNatural :: forall n . KnownNat n => Natural
natToNatural = snatToNatural (SNat @n)
{-# INLINE natToNatural #-}

-- | Reify the type-level 'Nat' @n@ to it's term-level 'Natural'.
snatToNatural :: SNat n -> Natural
snatToNatural = naturalFromInteger . snatToInteger
{-# INLINE snatToNatural #-}

-- | Same as 'snatToNum', but doesn't take term arguments. Example usage:
--
-- >>> natToNum @5 @Int
-- 5
natToNum :: forall n a . (Num a, KnownNat n) => a
natToNum = snatToNum (SNat @n)
{-# INLINE natToNum #-}

-- | Reify the type-level 'Nat' @n@ to it's term-level 'Num'ber.
snatToNum :: forall a n . Num a => SNat n -> a
snatToNum p@SNat = fromInteger (snatToInteger p)
{-# INLINE snatToNum #-}

-- | Unary representation of a type-level natural
--
-- __NB__: Not synthesizable
data UNat :: Nat -> Type where
  UZero :: UNat 0
  USucc :: UNat n -> UNat (n + 1)

instance KnownNat n => Show (UNat n) where
  show x = 'u':show (natVal x)

-- | Convert a singleton natural number to its unary representation
--
-- __NB__: Not synthesizable
toUNat :: forall n . SNat n -> UNat n
toUNat p@SNat = case cmpNat (SNat @1) p of
  LTI -> USucc (toUNat @(n - 1) (predSNat p))
  EQI -> USucc UZero
  GTI -> case sameNat p (SNat @0) of
    Just Refl -> UZero
    _ -> error "toUNat: impossible: 1 > n and n /= 0 for (n :: Nat)"

-- | Convert a unary-encoded natural number to its singleton representation
--
-- __NB__: Not synthesizable
fromUNat :: UNat n -> SNat n
fromUNat UZero     = SNat :: SNat 0
fromUNat (USucc x) = addSNat (fromUNat x) (SNat :: SNat 1)

-- | Add two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
addUNat :: UNat n -> UNat m -> UNat (n + m)
addUNat UZero     y     = y
addUNat x         UZero = x
addUNat (USucc x) y     = USucc (addUNat x y)

-- | Multiply two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
mulUNat :: UNat n -> UNat m -> UNat (n * m)
mulUNat UZero      _     = UZero
mulUNat _          UZero = UZero
mulUNat (USucc x) y      = addUNat y (mulUNat x y)

-- | Power of two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
powUNat :: UNat n -> UNat m -> UNat (n ^ m)
powUNat _ UZero     = USucc UZero
powUNat x (USucc y) = mulUNat x (powUNat x y)

-- | Predecessor of a unary-encoded natural number
--
-- __NB__: Not synthesizable
predUNat :: UNat (n+1) -> UNat n
predUNat (USucc x) = x
#if __GLASGOW_HASKELL__ < 912
predUNat UZero     =
  error "predUNat: impossible: 0 minus 1, -1 is not a natural number"
#endif

-- | Subtract two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
subUNat :: UNat (m+n) -> UNat n -> UNat m
subUNat x         UZero     = x
subUNat (USucc x) (USucc y) = subUNat x y
#if __GLASGOW_HASKELL__ < 912
subUNat UZero     _         = error "subUNat: impossible: 0 + (n + 1) ~ 0"
#endif

-- | Predecessor of a singleton natural number
predSNat :: SNat (a+1) -> SNat (a)
predSNat SNat = SNat
{-# INLINE predSNat #-}

-- | Successor of a singleton natural number
succSNat :: SNat a -> SNat (a+1)
succSNat SNat = SNat
{-# INLINE succSNat #-}

-- | Add two singleton natural numbers
addSNat :: SNat a -> SNat b -> SNat (a+b)
addSNat SNat SNat = SNat
{-# INLINE addSNat #-}
infixl 6 `addSNat`

-- | Subtract two singleton natural numbers
subSNat :: SNat (a+b) -> SNat b -> SNat a
subSNat SNat SNat = SNat
{-# INLINE subSNat #-}
infixl 6 `subSNat`

-- | Multiply two singleton natural numbers
mulSNat :: SNat a -> SNat b -> SNat (a*b)
mulSNat SNat SNat = SNat
{-# INLINE mulSNat #-}
infixl 7 `mulSNat`

-- | Power of two singleton natural numbers
powSNat :: SNat a -> SNat b -> SNat (a^b)
powSNat SNat SNat = SNat
{-# OPAQUE powSNat #-}
infixr 8 `powSNat`

-- | Division of two singleton natural numbers
divSNat :: (1 <= b) => SNat a -> SNat b -> SNat (Div a b)
divSNat SNat SNat = SNat
{-# INLINE divSNat #-}
infixl 7 `divSNat`

-- | Modulo of two singleton natural numbers
modSNat :: (1 <= b) => SNat a -> SNat b -> SNat (Mod a b)
modSNat SNat SNat = SNat
{-# INLINE modSNat #-}
infixl 7 `modSNat`

-- | Power of two of a singleton natural number
pow2SNat :: SNat a -> SNat (2^a)
pow2SNat SNat = SNat
{-# INLINE pow2SNat #-}

-- | Ordering relation between two Nats
data SNatLE a b where
  SNatLE :: forall a b . a <= b => SNatLE a b
  SNatGT :: forall a b . (b+1) <= a => SNatLE a b

deriving instance Show (SNatLE a b)

-- | Get an ordering relation between two SNats
compareSNat :: forall a b . SNat a -> SNat b -> SNatLE a b
compareSNat a@SNat b@SNat = case cmpNat a b of
  LTI -> SNatLE
  EQI -> SNatLE
  GTI -> case cmpNat (succSNat b) a of
    LTI -> SNatGT
    EQI -> SNatGT
    GTI -> error "compareSNat: impossible: a > b and b + 1 > a"

