{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Adapted from https://hackage.haskell.org/package/fin-0.3.2/docs/src/Data.Fin.html
module Data.Fin
  ( Fin(..)
  , explicitShow
  , explicitShowsPrec
  , cataFin
  , isMin
  ) where

import Control.DeepSeq (NFData (..))
import Data.Kind
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Exception (ArithException (..), throw)
import GHC.TypeLits hiding (SNat)
import Test.QuickCheck qualified as QC
import Unsafe.Coerce (unsafeCoerce)

natSize :: forall n a. (KnownNat n, Num a) => a
natSize = fromInteger $ natVal $ Proxy @n

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

-- | Finite numbers: @[0..n-1]@.
type Fin :: Nat -> Type
data Fin n where
  FZ :: (1 <= n) => Fin n
  FS :: (1 <= n) => !(Fin (n - 1)) -> Fin n
  deriving stock (Typeable)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)

-- | 'Fin' is printed as 'Natural'.
--
-- To see explicit structure, use 'explicitShow' or 'explicitShowsPrec'
instance Show (Fin n) where
  showsPrec d  = showsPrec d . toNatural

type Induction :: Nat -> Constraint
class KnownNat n => Induction n where
  induction :: f 0 -> (forall m. (KnownNat m, Induction m) => f m -> f (m + 1)) -> f n

instance {-# OVERLAPPING #-} Induction 0 where
  induction n _ = n

instance {-# OVERLAPPABLE #-} (m ~ n + 1, Induction n, KnownNat m) => Induction m where
  induction n c = c (induction n c)

-- | Operations module @n@.
--
-- >>> map fromInteger [0, 1, 2, 3, 4, -5] :: [Fin N.Nat3]
-- [0,1,2,0,1,1]
--
-- >>> fromInteger 42 :: Fin N.Nat0
-- *** Exception: divide by zero
-- ...
--
-- >>> signum (FZ :: Fin N.Nat1)
-- 0
--
-- >>> signum (3 :: Fin N.Nat4)
-- 1
--
-- >>> 2 + 3 :: Fin N.Nat4
-- 1
--
-- >>> 2 * 3 :: Fin N.Nat4
-- 2
--
instance (Induction n) => Num (Fin n) where
  abs = id

  signum FZ          = FZ
  signum (FS FZ)     = FS FZ
  signum (FS (FS _)) = FS FZ

  fromInteger = unsafeFromNum . flip mod (natSize @n)

  n + m = fromInteger (toInteger n + toInteger m)
  n * m = fromInteger (toInteger n * toInteger m)
  n - m = fromInteger (toInteger n - toInteger m)

  negate = fromInteger . negate . toInteger

instance (Induction n) => Real (Fin n) where
  toRational = cataFin 0 succ

-- | 'quot' works only on @'Fin' n@ where @n@ is prime.
instance (Induction n) => Integral (Fin n) where
  toInteger = cataFin 0 succ

  quotRem a b = (quot a b, 0)
  quot a b = a * inverse b

-- | Multiplicative inverse.
--
-- Works for @'Fin' n@ where @n@ is coprime with an argument, i.e. in general when @n@ is prime.
--
-- >>> map inverse universe :: [Fin N.Nat5]
-- [0,1,3,2,4]
--
-- >>> zipWith (*) universe (map inverse universe) :: [Fin N.Nat5]
-- [0,1,1,1,1]
--
-- Adaptation of [pseudo-code in Wikipedia](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers)
--
inverse :: forall n. (Induction n) => Fin n -> Fin n
inverse = fromInteger . iter 0 n 1 . toInteger where
  n = natSize @n

  iter t _ _  0
    | t < 0     = t + n
    | otherwise = t
  iter t r t' r' =
    let q = r `div` r'
    in iter t' r' (t - q * t') (r - q * r')

instance (Induction n) => Enum (Fin n) where
  fromEnum = go where
    go :: Fin m -> Int
    go FZ     = 0
    go (FS n) = succ (go n)

  toEnum = unsafeFromNum @n @Int

instance (n ~ 1 + m, Induction m) => Bounded (Fin n) where
  minBound = FZ
  maxBound = getMaxBound $ induction
    (MaxBound FZ)
    (MaxBound . FS . getMaxBound)

newtype MaxBound n = MaxBound { getMaxBound :: Fin (1 + n) }

instance NFData (Fin n) where
  rnf FZ     = ()
  rnf (FS n) = rnf n

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

instance (n ~ 1 + m, Induction m) => QC.Arbitrary (Fin n) where
  arbitrary = getArb $ induction (Arb $ pure FZ) step where
    step :: forall p. KnownNat p => Arb p -> Arb (1 + p)
    step (Arb p) = Arb $ QC.frequency
        [ (1,              pure FZ)
        , (1 + natSize @p, fmap FS p)
        ]

  shrink = shrink

shrink :: Fin n -> [Fin n]
shrink FZ      = []
shrink (FS FZ) = [FZ]
shrink (FS n)  = map FS (shrink n)

newtype Arb n = Arb { getArb :: QC.Gen (Fin (1 + n)) }

instance QC.CoArbitrary (Fin n) where
  coarbitrary FZ     = QC.variant (0 :: Int)
  coarbitrary (FS n) = QC.variant (1 :: Int) . QC.coarbitrary n

instance (n ~ 1 + m, KnownNat m) => QC.Function (Fin n) where
  function = case snat @m of
    SZ -> QC.functionMap (\_ -> ()) (\() -> FZ)
    SS -> QC.functionMap isMin      (maybe FZ FS)

type SNat :: Nat -> Type
data SNat n where
  SZ :: SNat 0
  SS :: SNat (1 + n)

snat :: forall n. KnownNat n => SNat n
snat =
  case natSize @n of
    0 -> unsafeCoerce SZ
    _ -> unsafeCoerce SS


-------------------------------------------------------------------------------
-- Showing
-------------------------------------------------------------------------------

-- | 'show' displaying a structure of 'Fin'.
--
-- >>> explicitShow (0 :: Fin N.Nat1)
-- "FZ"
--
-- >>> explicitShow (2 :: Fin N.Nat3)
-- "FS (FS FZ)"
--
explicitShow :: Fin n -> String
explicitShow n = explicitShowsPrec 0 n ""

-- | 'showsPrec' displaying a structure of 'Fin'.
explicitShowsPrec :: Int -> Fin n -> ShowS
explicitShowsPrec _ FZ     = showString "FZ"
explicitShowsPrec d (FS n) = showParen (d > 10)
    $ showString "FS "
    . explicitShowsPrec 11 n

-- -- -------------------------------------------------------------------------------
-- -- -- Conversions
-- -- -------------------------------------------------------------------------------

-- | Fold 'Fin'.
cataFin :: forall a n. a -> (a -> a) -> Fin n -> a
cataFin z f = go where
  go :: Fin m -> a
  go FZ = z
  go (FS n) = f (go n)

-- | Convert to 'Natural'.
toNatural :: Fin n -> Natural
toNatural = cataFin 0 succ

-- | Convert from any 'Ord' 'Num'.
unsafeFromNum :: forall n i. (Num i, Ord i, Induction n) => i -> Fin n
unsafeFromNum = appUnsafeFromNum (induction start step) where
  start :: UnsafeFromNum i 0
  start = UnsafeFromNum $ \n -> case compare n 0 of
    LT -> throw Underflow
    EQ -> throw Overflow
    GT -> throw Overflow

  step :: UnsafeFromNum i m -> UnsafeFromNum i (1 + m)
  step (UnsafeFromNum f) = UnsafeFromNum $ \n -> case compare n 0 of
    EQ -> FZ
    GT -> FS (f (n - 1))
    LT -> throw Underflow

newtype UnsafeFromNum i n = UnsafeFromNum { appUnsafeFromNum :: i -> Fin n }

-- | Return a one less.
--
-- >>> isMin (FZ :: Fin N.Nat1)
-- Nothing
--
-- >>> map isMin universe :: [Maybe (Fin N.Nat3)]
-- [Nothing,Just 0,Just 1,Just 2]
--
-- @since 0.1.1
--
isMin :: Fin (1 + n) -> Maybe (Fin n)
isMin FZ     = Nothing
isMin (FS n) = Just n

