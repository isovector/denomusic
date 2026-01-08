{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wtype-defaults   #-}

module DenoMusic.Harmony (
  T (..),
  extend,
  sink,
  kill,
  elim,
  MetaScales (..),
  MetaScale (..),

  -- * Familiar objects
  triad,
  diatonic,
  spelledFlat,
  spelledSharp,
  standardSharp,
  standardFlat,
  vl3in7,
  vl7in12,
) where

import Data.Bool
import Data.Ratio
import Data.Group
import Data.Kind
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as S
import DenoMusic.Types (PitchClass (..), Reg (..))
import GHC.Exts
import GHC.TypeLits

-- | A coordinate inside of a 'MetaScales'. 'T's are little-endian cons lists.
-- For example, given the 'standard' 'MetaScale', @1 :> (-2) :> 3 :> Nil@ means
-- to transpose up by one chord tone, down by two scale tones, and up by three
-- chromatic tones.
type T :: [Nat] -> Type
data T sizes where
  Nil :: T '[]
  (:>) :: Int -> !(T ns) -> T (n ': ns)

infixr 6 :>

deriving stock instance Eq (T ns)
deriving stock instance Ord (T ns)
deriving stock instance Show (T ns)

instance IsList (T '[]) where
  type Item (T '[]) = Int
  fromList [] = Nil
  fromList e = error $ "Extra items remaining in IsList (T '[]): " <> show e
  toList Nil = []

instance (IsList (T ns), Item (T ns) ~ Int) => IsList (T (n ': ns)) where
  type Item (T (n ': ns)) = Int
  fromList [] = error "Not enough items in IsList (T (n ': ns)): "
  fromList (x : xs) = x :> fromList xs
  toList (x :> xs) = x : toList xs

instance Semigroup (T '[]) where
  _ <> _ = Nil

instance Semigroup (T ns) => Semigroup (T (n ': ns)) where
  (i :> is) <> (j :> js) = (i + j) :> (is <> js)

instance Monoid (T '[]) where
  mempty = Nil

instance Monoid (T ns) => Monoid (T (n ': ns)) where
  mempty = 0 :> mempty

instance Group (T '[]) where
  invert _ = Nil

instance Group (T ns) => Group (T (n ': ns)) where
  invert (i :> is) = negate i :> invert is

-- | 'MetaScales' provide consistent vertical musical constraints (harmony), as
-- well as give means for efficient voice leading in order to evolve that
-- harmony over time.
--
-- Musically, a 'MetaScales' is a hierarchy of scale-like things which move
-- relative to one another. Think chord-inside-scale-inside-modulation. The
-- @sizes@ type index describes how many elements is in each of these
-- scale-like things, in little-endian.
--
-- You can index into a 'MetaScales' by way of a 'T'.
type MetaScales :: [Nat] -> Type -> Type
data MetaScales sizes a where
  -- | The base collection of objects being permuted.
  Base :: Set a -> MetaScales '[n] a
  -- | Transform a 'MetaScales' by permuting it via a single 'MetaScale'.
  MSCons :: MetaScale n -> !(MetaScales ns a) -> MetaScales (n ': ns) a

deriving stock instance Show a => Show (MetaScales ns a)

-- | A 'MetaScale' is a collection of generalized scale-steps, relative to
-- a parent 'MetaScale'. It can be used to describe voices-within-chords, or
-- chords-within-scales, or scales-within-chroma, or other things of this
-- nature. A 'MetaScale' is mapped to concrete values when embedded within
-- a 'MetaScales' (notice the plural.)
type MetaScale :: Nat -> Type
newtype MetaScale size = UnsafeMetaScale
  { getMetaScale :: Set Int
  }
  deriving newtype (Show)

-- | The diatonic scale. This will take on different modes depending on the
-- background scalar transposition applied to it.
diatonic :: MetaScale 7
diatonic = UnsafeMetaScale $ S.fromList [0, 2, 4, 5, 7, 9, 11]

-- | A metascale corresponding to the 1-3-5 triad. This will take on
-- major/minor/diminished/augmented characteristics depending on where in the
-- scale it is transposed to.
triad :: MetaScale 3
triad = UnsafeMetaScale $ S.fromList [0, 2, 4]

-- | Transform a note along a 'MetaScales' by moving it along each scale
-- dimension. This function forms monoid actions:
--
-- @
-- elim ms mempty     = id
-- elim ms (t1 <> t2) = elim ms t2 . elim ms t1
-- @
elim :: Ord a => MetaScales ns a -> Reg a -> T ns -> Reg a
elim (Base sc) r (i :> Nil) = metaMove sc i r
elim (MSCons ms scs) r (i :> j :> js) = do
  dj <- metaMove (getMetaScale ms) i (Reg 0 0)
  elim scs r ((dj + j) :> js)

kill :: forall n m ns. KnownNat m => MetaScale n -> T (n ': m ': ns) -> T (m ': ns)
kill ms (i :> j :> js) =
  let (Reg z dj) = metaMove (getMetaScale ms) i (Reg 0 0)
   in ((dj + j + z * fromIntegral (natVal (Proxy @m))) :> js)

-- | The standard triad-in-diatonic-in-chromatic 'MetaScales' that makes up
-- most of Western music.
standardFlat :: MetaScales '[3, 7, 12] PitchClass
standardFlat = MSCons triad $ MSCons diatonic spelledFlat

-- | The standard triad-in-diatonic-in-chromatic 'MetaScales' that makes up
-- most of Western music.
standardSharp :: MetaScales '[3, 7, 12] PitchClass
standardSharp = MSCons triad $ MSCons diatonic spelledSharp

-- | A chromatic 'MetaScales' that spells its enharmonic black notes as sharps.
spelledSharp :: MetaScales '[12] PitchClass
spelledSharp = Base (S.fromList [A, As, B, C, Cs, D, Ds, E, F, Fs, G, Gs])

-- | A chromatic 'MetaScales' that spells its enharmonic black notes as flats.
spelledFlat :: MetaScales '[12] PitchClass
spelledFlat = Base (S.fromList [A, Af, B, Bf, C, D, Df, E, Ef, F, G, Gf])

-- | A smooth downwards voice-leading of triads-in-diatonic. Use 'invert' to
-- instead get an upwards voice-leading.
vl3in7 :: T '[3, 7]
vl3in7 = (-2) :> 5 :> mempty

-- | A smooth downwards voice-leading of diatonics-in-chromatics. Use 'invert'
-- to instead get an upwards voice-leading.
vl7in12 :: T '[7, 12]
vl7in12 = (-4) :> 7 :> mempty

type family (++) xs ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Extend the end of a 'T' with zeroes.
extend :: forall ns ms. Monoid (T ns) => T ms -> T (ms ++ ns)
extend (x :> xs) = x :> extend @ns xs
extend Nil = mempty

-- | Extend the front of a 'T' with a zero.
sink :: T ns -> T (n ': ns)
sink t = 0 :> t

-- | Like 'pred', but over the metascale distance metric.
metaPred :: Ord a => Set a -> Reg a -> Reg a
metaPred sc (Reg r a)
  | a == S.findMin sc = Reg (r - 1) $ S.findMax sc
  | otherwise = Reg r $ S.findMax $ snd $ S.partition (>= a) sc

-- | Like 'succ', but over the metascale distance metric.
metaSucc :: Ord a => Set a -> Reg a -> Reg a
metaSucc sc (Reg r a)
  | a == S.findMax sc = Reg (r + 1) $ S.findMin sc
  | otherwise = Reg r $ S.findMin $ snd $ S.partition (<= a) sc

-- | Iterated 'pred' or 'succ' over the metascale distance metric.
metaMove :: Ord a => Set a -> Int -> Reg a -> Reg a
metaMove sc n r =
  case compare n 0 of
    LT -> iterate (metaPred sc) r !! abs n
    EQ -> r
    GT -> iterate (metaSucc sc) r !! abs n


newtype VoiceLeading x y = VoiceLeading
  { unVoiceLeading :: Int
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

toT :: forall x y. (KnownNat x, KnownNat y) => VoiceLeading x y -> T '[x, y]
toT (VoiceLeading i) =
  let x = natVal (Proxy @x)
      y = natVal (Proxy @y)

      angle_offset = x % y

      -- First wrap i to [0, y)
      i_wrapped = fromIntegral $ mod (fromIntegral i) y

      -- Calculate angular position as fraction of full rotation
      angle = fromIntegral i_wrapped * angle_offset
      fractional = angle - fromIntegral (truncate @_ @Int angle)

      -- Wrap to (-0.5, 0.5] to determine direction
      wrapped = bool id (subtract 1) (fractional > 0.5) fractional

      -- If moving counterclockwise (wrapped < 0), subtract scaleSize
      tLevel = bool id (subtract y) (wrapped < 0) i_wrapped

      -- Solve for sTrans to minimize voice leading
      sTrans = negate $ round (fromIntegral tLevel * angle_offset)
   in sTrans :> fromIntegral tLevel :> Nil
