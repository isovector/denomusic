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
import Music.Types.Ornaments


type Note :: Nat -> Type
data Note n = Note (Deg n) (Ornaments Int)
  deriving stock (Generic, Eq, Ord)

instance Num (Note n) where
  fromInteger n = Note (fromInteger n) mempty
  Note a as + Note b bs = Note (a + b) $ as <> bs
  Note a as - Note b bs = Note (a - b) $ as <> fmap negate bs
  Note a (Ornaments as) * Note b (Ornaments bs) =
    Note (a * b) $ Ornaments $ IM.intersectionWith (*) as bs
  abs    (Note a as) = Note (abs a)    $ fmap abs    as
  signum (Note a as) = Note (signum a) $ fmap signum as

instance Show (Note n) where
  showsPrec p (Note n (Ornaments m))
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


stepIn :: forall c n. KnownNat c => Int -> Note n -> Note n
stepIn k (Note a as) = Note a $ as <> ornament @c k


stepsIn :: [(Int, Int)] -> Note n -> Note n
stepsIn ks (Note a as) = Note a $ as <> Ornaments (IM.fromListWith (+) ks)


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

