{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns        #-}

module DenoMusic.Harmony2 where

import Control.Category
import Prelude hiding (id, (.))
import Data.Align
import Data.Bool
import Data.Kind
import Data.List (dropWhileEnd, sort, nub)
import Data.Proxy
import Data.Ratio
import Data.These
import GHC.TypeLits

type Deg :: Nat -> Type
data Deg n = MkDeg Int [Int]

scaleSize :: forall n. KnownNat n => Int
scaleSize = fromInteger $ natVal $ Proxy @n


pattern Deg :: Int -> [Int] -> Deg n
pattern Deg a as <- MkDeg a (dropWhileEnd (== 0) -> as)
  where
    Deg a = MkDeg a . dropWhileEnd (== 0)
{-# COMPLETE Deg #-}

normalizeDeg :: forall n. KnownNat n => Deg n -> Deg n
normalizeDeg (Deg a as) = Deg (mod a $ scaleSize @n) as


instance Eq (Deg n) where
  Deg a as == Deg b bs = a == b && as == bs

instance Ord (Deg n) where
  compare (Deg a as) (Deg b bs)
    = foldMap
        (these (flip compare 0) (compare 0) compare)
    $ align (a : as) (b : bs)

instance Num (Deg n) where
  fromInteger n = Deg (fromInteger n) []
  Deg a as + Deg b bs = Deg (a + b) $ alignWith (these id id (+)) as bs
  Deg a as - Deg b bs = Deg (a - b) $ alignWith (these id id (-)) as bs
  Deg a as * Deg b bs = Deg (a * b) $ alignWith (these id id (*)) as bs
  abs    (Deg a as) = Deg (abs a)    $ fmap abs    as
  signum (Deg a as) = Deg (signum a) $ fmap signum as

instance Show (Deg n) where
  showsPrec p (Deg n []) = showsPrec p n
  showsPrec p (Deg n ns) = showParen (p >= 11) $ showsPrec 11 n . showChar '→' . showsPrec 11 ns

data Scale a b = Scale
  { applyScale :: Deg a -> Deg b
  }

instance Category Scale where
  id = Scale id
  Scale g . Scale f = Scale $ g . f

elim :: (Int -> Deg b) -> Deg a -> Deg b
elim f (Deg a []) = f a
elim f (Deg a (j : js)) = f a + Deg j js

mkScale :: forall c s. (KnownNat c, KnownNat s) => [Deg s] -> Scale c s
mkScale ds = do
  let ds' = nub $ sort $ fmap normalizeDeg ds
      o = scaleSize @s
      n = length ds'
  case n == scaleSize @c of
    True ->
      Scale $ elim $ \i -> do
        let (q, r) = divMod i n
        (ds' !! r) + fromIntegral (q * o)
    False -> error $ unwords
      [ "mkScale: declared as chord size"
      , show $ scaleSize @c
      , "but given"
      , show n
      , "notes."
      ]

triad :: Scale 3 7
triad = mkScale [0, 2, 4]

diatonic :: Scale 7 12
diatonic = mkScale [0, 2, 4, 5, 7, 9, 11]


data T c s = T
  { intrinsic :: Deg c
  , extrinsic :: Deg s
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup (T c s) where
  T a1 b1 <> T a2 b2 = T (a1 + a2) (b1 + b2)

instance Monoid (T c s) where
  mempty = T 0 0

instance Num (T c s) where
  T a1 b1 + T a2 b2 = T (a1 + a2) (b1 + b2)
  T a1 b1 - T a2 b2 = T (a1 - a2) (b1 - b2)
  T a1 b1 * T a2 b2 = T (a1 * a2) (b1 * b2)
  abs    (T a1 b1) = T (abs a1)    (abs b1)
  signum (T a1 b1) = T (signum a1) (signum b1)
  fromInteger n = T (fromInteger n) (fromInteger n)

lead :: forall c s. (KnownNat c, KnownNat s) => Deg s -> Deg s -> T c s
lead (Deg from _) (Deg to _) =
  let i = to - from
      c = scaleSize @c
      s = scaleSize @s

      angle_offset = c % s

      -- First wrap i to [0, s)
      i_wrapped = mod i s

      -- Calculate angular position as fraction of full rotation
      angle = fromIntegral i_wrapped * angle_offset
      fractional = angle - fromIntegral (truncate @_ @Int angle)

      -- Wrap to (-0.5, 0.5] to determine direction
      wrapped = bool id (subtract 1) (fractional > 0.5) fractional

      -- If moving counterclockwise (wrapped < 0), subtract scaleSize
      tLevel = bool id (subtract s) (wrapped < 0) i_wrapped

      -- Solve for sTrans to minimize voice leading
      sTrans = negate $ round (fromIntegral tLevel * angle_offset)
   in T (fromIntegral sTrans) $ fromIntegral tLevel

apply :: T c s -> Scale c s -> Scale c s
apply (T i o) (Scale f) = Scale $ (+ o) . f . (+ i)

