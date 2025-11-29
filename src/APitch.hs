{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module APitch
  ( APitch (..)
  , up
  , down
  , sharp
  , flat
  , inverted

  -- * Intervals
  , tonic
  , second
  , third
  , fourth
  , fourth'
  , fifth
  , sixth
  , sixth'
  , seventh
  , eighth
  , ninth
  , octave

  -- * Scales
  , ScaleForm (..)
  , major
  , naturalMinor
  , harmonicMinor
  , klezmer
  , hungarianMajor
  , hungarianMinor

  -- * Modes
  , ionian
  , dorian
  , phrygian
  , lydian
  , mixolydian
  , aeolian
  , locrian

  -- * Chords
  , invert

  -- * Utils
  , nextPitchSize
  ) where

import Control.DeepSeq
import GHC.Generics
import Data.Music.Lilypond.Pitch
import Euterpea qualified as E

instance Semigroup APitch where
  AP o1 n1 a1 <> AP o2 n2 a2 = AP (o1 + o2) (n1 + n2) (a1 + a2)

instance Monoid APitch where
  mempty = AP 0 0 0

tonic :: APitch
tonic = mempty

second :: APitch
second = AP 0 1 0

third :: APitch
third = AP 0 2 0

fourth :: APitch
fourth = AP 0 3 0

fourth' :: APitch
fourth' = AP 0 0 5

fifth :: APitch
fifth = AP 0 4 0

sixth :: APitch
sixth = AP 0 5 0

sixth' :: APitch
sixth' = AP 0 0 8

seventh :: APitch
seventh = AP 0 6 0

-- | An eighth is not necessarily an octave in every scale!
eighth :: APitch
eighth = AP 0 7 0

octave :: APitch
octave = AP 1 0 0

ninth :: APitch
ninth = AP 0 8 0

inverted :: APitch -> APitch
inverted (AP r x y) = AP (negate r) (negate x) (negate y)


toEAbsPitch :: Pitch -> Int
toEAbsPitch (Pitch (n, a, o)) =
  E.absPitch (toEPitchClass n, o) + a

instance E.ToMusic1 Pitch where
  toMusic1 = E.toMusic1 . fmap toEAbsPitch


toEPitchClass :: PitchName -> E.PitchClass
toEPitchClass C = E.C
toEPitchClass D = E.D
toEPitchClass E = E.E
toEPitchClass F = E.F
toEPitchClass G = E.G
toEPitchClass A = E.A
toEPitchClass B = E.B

data ScaleForm = ScaleForm
  { unScale :: [Int]
  , s_name :: String
  }
  deriving stock (Eq, Ord, Show)


-- | Abstract pitches, capable of differentiating between register, scale
-- degree, and accidentals. This can be transformed into a "real" pitch by
-- combinining it against a 'Scale' to differentiate the scale degrees. This
-- operation will engrave with the proper enharmonic encodings for the given
-- 'Scale'.
--
-- Many of the usual musical facts /do not hold/ for 'APitch'. For example, it
-- is not the case that an octave corresponds to raising by an eighth. To
-- illustrate this fact, an 'eighth' always corresponds to eight scale degrees,
-- which is not the same thing as an octave in a pentatonic scale.
data APitch = AP
  { a_register :: Int
  , a_num :: Int
  , a_accidental :: Int
  }
  deriving stock (Eq, Ord)

instance Show APitch where
  show (AP r n o) =
    mconcat
      [ case compare o 0 of
          LT -> replicate (negate o) '♭'
          EQ -> "♮"
          GT -> replicate o '♯'
      , show $ n + signum n
      , case compare r 0 of
          LT -> replicate (negate r) 'ᵇ'
          EQ -> "♮"
          GT -> replicate r 'ᵃ'
      ]

modal :: Int -> ScaleForm -> ScaleForm
modal n (ScaleForm is m) = ScaleForm (drop n is <> take n is) $ m <> "◦" <> show (n + 1)

klezmer :: ScaleForm
klezmer = ScaleForm [1, 3, 1, 2, 1, 2, 2] "Kz"


hungarianMajor :: ScaleForm
hungarianMajor = ScaleForm [3, 1, 2, 1, 2, 1, 2] "Hg"

hungarianMinor :: ScaleForm
hungarianMinor = ScaleForm [2, 1, 3, 1, 1, 3, 1] "hg"

major :: ScaleForm
major = ScaleForm [2, 2, 1, 2, 2, 2, 1] ""

naturalMinor :: ScaleForm
naturalMinor = aeolian major

harmonicMinor :: ScaleForm
harmonicMinor = ScaleForm [2, 1, 2, 2, 1, 3, 2] "◦6♮7"

ionian :: ScaleForm -> ScaleForm
ionian = modal 0

dorian :: ScaleForm -> ScaleForm
dorian = modal 1

phrygian :: ScaleForm -> ScaleForm
phrygian = modal 2

lydian :: ScaleForm -> ScaleForm
lydian = modal 3

mixolydian :: ScaleForm -> ScaleForm
mixolydian = modal 4

aeolian :: ScaleForm -> ScaleForm
aeolian = modal 5

locrian :: ScaleForm -> ScaleForm
locrian = modal 6

up :: APitch -> APitch
up (AP o x _) = AP o (succ x) 0

down :: APitch -> APitch
down (AP o x _) = AP o (pred x) 0

sharp :: APitch -> APitch
sharp x = x { a_accidental = a_accidental x + 1 }

flat :: APitch -> APitch
flat x = x { a_accidental = a_accidental x - 1 }

invert :: [APitch] -> [APitch]
invert [] = []
invert (x : xs) = xs ++ [x <> octave]

deriving stock instance Generic Pitch
deriving anyclass instance NFData Pitch
deriving stock instance Generic PitchName
deriving anyclass instance NFData PitchName

nextPitchSize :: PitchName -> Int
nextPitchSize E = 1
nextPitchSize B = 1
nextPitchSize _ = 2

