{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns        #-}

module Music.Types.Note
  ( module Music.Types.Note
  , KnownNat
  ) where

import Data.IntMap qualified as IM
import Data.Kind
import Data.Semigroup (Sum(..))
import GHC.Generics (Generic)
import GHC.TypeLits
import Music.Types.Deg
import Music.Types.Accidentals


type Note :: Nat -> Type
data Note n = Note (Deg n) (Accidentals Int)
  deriving stock (Generic, Eq, Ord)

instance Num (Note n) where
  fromInteger n = Note (fromInteger n) mempty
  Note a as + Note b bs = Note (a + b) $ as <> bs
  Note a as - Note b bs = Note (a - b) $ as <> fmap negate bs
  Note a (Accidentals as) * Note b (Accidentals bs) =
    Note (a * b) $ Accidentals $ IM.intersectionWith (*) as bs
  abs    (Note a as) = Note (abs a)    $ fmap abs    as
  signum (Note a as) = Note (signum a) $ fmap signum as

instance Show (Note n) where
  showsPrec p (Note n (Accidentals m))
    | IM.null m = showsPrec p n
    | otherwise = showParen (p >= 11)
        $ showString "stepsIn "
        . showsPrec 11 (IM.toList m)
        . showChar ' '
        . showsPrec 11 n


noteDeg :: Note n -> Deg n
noteDeg (Note d _) = d


normalizeNote :: forall n. KnownNat n => Note n -> Note n
normalizeNote (Note (Deg a) as) = Note (Deg $ mod a $ scaleSize @n) as


stepIn :: forall c n. (KnownNat c, KnownNat n) => Int -> Note n -> Note n
stepIn k (Note a as) =
  case scaleSize @c == scaleSize @n of
    True -> Note (a + Deg k) as
    False -> Note a $ as <> accidental @c k


stepsIn :: [(Int, Int)] -> Note n -> Note n
stepsIn ks (Note a as) = Note a $ as <> Accidentals (IM.fromListWith (+) ks)


dropAccidental :: forall c n. KnownNat c => Note n -> Note n
dropAccidental (Note n o) = Note n $ fst $ decompAccidental @c o


newtype Octave = Octave
  { getOct :: Int
  }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)
  deriving (Semigroup, Monoid) via Sum Int


addOctave :: forall n. KnownNat n => Octave -> Note n -> Note n
addOctave o n = n + fromIntegral (getOct o * scaleSize @n)

inOctave :: forall n. KnownNat n => Octave -> Note n -> Note n
inOctave o = addOctave o . normalizeNote

