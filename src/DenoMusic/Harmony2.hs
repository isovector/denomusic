{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns        #-}

module DenoMusic.Harmony2 where

import GHC.Generics (Generic)
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

type Note :: Nat -> Type
data Note n = MkNote (Deg n) [Int]
  deriving stock Generic


type Deg :: Nat -> Type
newtype Deg n = Deg
  { getDeg :: Int
  }
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Num)

scaleSize :: forall n. KnownNat n => Int
scaleSize = fromInteger $ natVal $ Proxy @n


pattern Note :: Deg n -> [Int] -> Note n
pattern Note a as <- MkNote a (dropWhileEnd (== 0) -> as)
  where
    Note a = MkNote a . dropWhileEnd (== 0)
{-# COMPLETE Note #-}

infixl 6 /:
(/:) :: Deg n -> [Int] -> Note n
(/:) = MkNote

infixl 6 /+
(/+) :: Note n -> Int -> Note n
a /+ b = a + Note 0 [b]

infixl 6 //+
(//+) :: Note n -> Int -> Note n
a //+ b = a + Note 0 [0, b]

infixl 6 /-
(/-) :: Note n -> Int -> Note n
a /- b = a - Note 0 [b]

infixl 6 //-
(//-) :: Note n -> Int -> Note n
a //- b = a - Note 0 [0, b]


normalizeNote :: forall n. KnownNat n => Note n -> Note n
normalizeNote (Note (Deg a) as) = Note (Deg $ mod a $ scaleSize @n) as


instance Eq (Note n) where
  Note a as == Note b bs = a == b && as == bs

instance Ord (Note n) where
  compare (Note (Deg a) as) (Note (Deg b) bs)
    = foldMap
        (these (flip compare 0) (compare 0) compare)
    $ align (a : as) (b : bs)

instance Num (Note n) where
  fromInteger n = Note (fromInteger n) []
  Note a as + Note b bs = Note (a + b) $ alignWith (these id        id        (+)) as bs
  Note a as - Note b bs = Note (a - b) $ alignWith (these id        negate    (-)) as bs
  Note a as * Note b bs = Note (a * b) $ alignWith (these (const 0) (const 0) (*)) as bs
  abs    (Note a as) = Note (abs a)    $ fmap abs    as
  signum (Note a as) = Note (signum a) $ fmap signum as

instance Show (Note n) where
  showsPrec p (Note n []) = showsPrec p n
  showsPrec p (Note n [a]) = showParen (p >= 11) $ showsPrec 11 n . showString (bool ("/-") ("/+") (a >= 0)) . showsPrec 11 (abs a)
  showsPrec p (Note n [0, a]) = showParen (p >= 11) $ showsPrec 11 n . showString (bool ("//-") ("//+") (a >= 0)) . showsPrec 11 (abs a)
  showsPrec p (Note n ns) = showParen (p >= 11) $ showsPrec 11 n . showString "/:" . showsPrec 11 ns

data Scale a b = Scale
  { applyScale :: Note a -> Note b
  }

instance Category Scale where
  id = Scale id
  Scale g . Scale f = Scale $ g . f

elim :: (Deg a -> Note b) -> Note a -> Note b
elim f (Note a []) = f a
elim f (Note a (j : js)) = f a + Note (Deg j) js

mkScale :: forall c s. (KnownNat c, KnownNat s) => [Note s] -> Scale c s
mkScale ds = do
  let ds' = nub $ sort $ fmap normalizeNote ds
      o = scaleSize @s
      n = length ds'
  case n == scaleSize @c of
    True ->
      Scale $ elim $ \(Deg i) -> do
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
  abs    (T a b) = T (abs a)    (abs b)
  signum (T a b) = T (signum a) (signum b)
  fromInteger n = T (fromInteger n) (fromInteger n)

lead :: forall c s. (KnownNat c, KnownNat s) => Deg s -> Deg s -> T c s
lead (Deg from) (Deg to) =
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
apply (T i o) (Scale f) = Scale $ (+ MkNote o []) . f . (+ MkNote i [])

