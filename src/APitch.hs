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

  -- * Observations
  , toPitch

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
  , Scale (..)
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
import Data.Semigroup
import Data.Bool
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

data Scale = Scale
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

modal :: Int -> Scale -> Scale
modal n (Scale is m) = Scale (drop n is <> take n is) $ m <> "◦" <> show (n + 1)

klezmer :: Scale
klezmer = Scale [1, 3, 1, 2, 1, 2, 2] "Kz"


hungarianMajor :: Scale
hungarianMajor = Scale [3, 1, 2, 1, 2, 1, 2] "Hg"

hungarianMinor :: Scale
hungarianMinor = Scale [2, 1, 3, 1, 1, 3, 1] "hg"

major :: Scale
major = Scale [2, 2, 1, 2, 2, 2, 1] ""

naturalMinor :: Scale
naturalMinor = aeolian major

harmonicMinor :: Scale
harmonicMinor = Scale [2, 1, 2, 2, 1, 3, 2] "◦6♮7"

ionian :: Scale -> Scale
ionian = modal 0

dorian :: Scale -> Scale
dorian = modal 1

phrygian :: Scale -> Scale
phrygian = modal 2

lydian :: Scale -> Scale
lydian = modal 3

mixolydian :: Scale -> Scale
mixolydian = modal 4

aeolian :: Scale -> Scale
aeolian = modal 5

locrian :: Scale -> Scale
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

va :: Int -> APitch -> APitch
va n p = stimes n octave <> p

vb :: Int -> APitch -> APitch
vb n p = stimes n (inverted octave) <> p

deriving stock instance Generic Pitch
deriving anyclass instance NFData Pitch
deriving stock instance Generic PitchName
deriving anyclass instance NFData PitchName

succPitch :: PitchName -> PitchName
succPitch B = C
succPitch x = succ x

predPitch :: PitchName -> PitchName
predPitch C = B
predPitch x = pred x

nextPitchSize :: PitchName -> Int
nextPitchSize E = 1
nextPitchSize B = 1
nextPitchSize _ = 2

toPitch :: Scale -> Pitch -> APitch -> Pitch
toPitch (Scale is _) = go $ cycle is
  where
    go _ (Pitch (c, a, o)) (AP r 0 da) = Pitch (c, a + da, o + r)
    go (x : xs) (Pitch (c, a, o)) (AP r dn da) = do
      let c' = succPitch c
      go
        xs
        (Pitch (c', a + x - nextPitchSize c, bool 0 1 (c' < c) + o))
        (AP r (dn - 1) da)
    go _ _ _ = error "impossible"

