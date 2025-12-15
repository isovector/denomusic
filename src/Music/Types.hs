module Music.Types
  ( module Music.Types
  , Reg(..)
  , fromReg
  , withReg
  , T(..)
  ) where

import qualified Data.Set as S
import Data.Maybe
import Data.Monoid
import Data.Monoid.Action
import Data.Set (Set)
import Data.Tree.DUAL
import Music.Harmony

data PitchClass
  = Cff | Cf | C | Cs | Css
  | Dff | Df | D | Ds | Dss
  | Eff | Ef | E | Es | Ess
  | Fff | Ff | F | Fs | Fss
  | Gff | Gf | G | Gs | Gss
  | Aff | Af | A | As | Ass
  | Bff | Bf | B | Bs | Bss
  deriving stock (Show, Eq, Ord, Read, Enum, Bounded)


data Envelope = Envelope
  { e_duration :: Rational
  , e_offset :: Rational
  , e_scale :: Last (Set PitchClass)
  , e_root :: Last (Reg PitchClass)
  , e_chord :: Last (Set Int)
  , e_harmony :: T
  , e_voice :: Last Int
  }

instance Semigroup Envelope where
  Envelope a1 b1 c1 d1 e1 f1 g1 <> Envelope a2 b2 c2 d2 e2 f2 g2
    = Envelope (a1 * a2) (a1 * b2 + b1) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2)

instance Monoid Envelope where
  mempty = Envelope 1 0 mempty mempty mempty mempty mempty

instance Action Envelope (Sum Rational) where
  act e (Sum s) = Sum $ e_duration e * s

data Annotation
  = -- | Change the time signature. This is expressed as two integers, rather
    -- than a rational, because time signatures do not normalize! For example,
    -- @3/4 /= 6/8@.
    TimeSignature
      -- | Numerator
      Integer
      -- | Denominator
      Integer
  | -- | Change the tempo.
    Tempo
      -- | Default note duration
      Rational
      -- | This many times per minute
      Integer
  | Phrase


newtype Music = Music
  { unMusic :: DUALTree Envelope (Sum Rational) Annotation T
  }


instance Semigroup Music where
  sa@(Music a) <> Music b =
    Music $ a <> applyD (mempty { e_offset = duration sa }) b

instance Monoid Music where
  mempty = Music mempty


addEnv :: Envelope -> Music -> Music
addEnv e = Music . applyD e . unMusic

addAnn :: Annotation -> Music -> Music
addAnn a = Music . annot a . unMusic

export
  :: Envelope
  -> T
  -> Reg PitchClass
export e t = do
  let sc = fromMaybe (S.fromList [C, D, E, F, G, A, B]) $ getLast $ e_scale e
      root = fromMaybe (Reg 4 C) $ getLast $ e_root e
      ch = S.map (flip (extrMove sc) root) $ fromMaybe (S.fromList [0, 2, 4])
         $ getLast
         $ e_chord e
      (virtual_v, v)
        = quotRem (fromMaybe 0 $ getLast $ e_voice e)
        $ length ch
  move1 sc ch (e_harmony e <> chordTone virtual_v <> t)
      $ (S.toList ch !!)
      $ v

duration :: Music -> Rational
duration = getSum . fromMaybe mempty . getU . unMusic
