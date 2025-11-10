{-# OPTIONS_GHC -fno-warn-x-partial #-}

module Theory.Chords
  ( chordic
  , invert
  , inversion
  , maj
  , power
  , minor
  , dim
  , maj7
  , min7
  , dom7

  , nthMode
  , ionianSemitones
  , addSemitonesToPitchClass
  , addAscendingOctave
  ) where

import Data.Bool (bool)
import Euterpea (PitchClass(..), Pitch, trans, Octave)


ionianSemitones :: [Int]
ionianSemitones = scanl (+) 0 [2, 2, 1, 2, 2, 2]


nthMode :: Int -> [a] -> [a]
nthMode n as = take (length as) $ drop (n - 1) $ cycle as


addSemitonesToPitchClass :: PitchClass -> Int -> PitchClass
addSemitonesToPitchClass pc st = fst $ trans st (pc, 4)


addAscendingOctave :: Octave -> [PitchClass] -> [Pitch]
addAscendingOctave _ [] = []
addAscendingOctave o [pc] = [(pc, o)]
addAscendingOctave o (pc1 : pcs@(pc2 : _)) =
  (pc1, o) : addAscendingOctave (o + bool 0 1 (pc2 < pc1)) pcs


dropEverySecond :: [a] -> [a]
dropEverySecond [] = []
dropEverySecond [a] = [a]
dropEverySecond (a1 : _ : as) = a1 : dropEverySecond as


-- | Helper function for generating tonal chords. Users are expected to curry
-- in the first 'PitchClass' in order to get a generator for their key.
chordic
  :: PitchClass  -- ^ tonic key
  -> Int  -- ^ mode
  -> Int  -- ^ chord degree
  -> Int  -- ^ chord type (eg 7th)
  -> Octave -- ^) register
  -> [Pitch]
chordic pc mode deg ty o
  = take ((ty + 1) `div` 2)
  $ dropEverySecond
  $ addAscendingOctave o
  $ fmap (addSemitonesToPitchClass pc)
  $ nthMode deg
  $ nthMode mode
  $ ionianSemitones


maj :: PitchClass -> Int -> [Pitch]
maj pc o = chordic pc 1 1 5 o

power :: PitchClass -> Int -> [Pitch]
power = curry $ \r ->
  [ r
  , trans 7 r
  , trans 12 r
  ]

minor :: PitchClass -> Int -> [Pitch]
minor pc o = chordic (addSemitonesToPitchClass pc 3) 1 6 5 o

dim :: PitchClass -> Int -> [Pitch]
dim pc o = chordic (addSemitonesToPitchClass pc 1) 1 7 5 o

maj7 :: PitchClass -> Int -> [Pitch]
maj7 pc o = chordic pc 1 1 7 o

min7 :: PitchClass -> Int -> [Pitch]
min7 pc o = chordic (addSemitonesToPitchClass pc 3) 1 6 7 o

dom7 :: PitchClass -> Int -> [Pitch]
dom7 pc o = chordic (addSemitonesToPitchClass pc 5) 1 5 7 o


invert :: [Pitch] -> [Pitch]
invert [] = []
invert ((pc, o) : ps) = ps <> [(pc, o + 1)]


inversion :: Int -> [Pitch] -> [Pitch]
inversion n = head . drop n . iterate invert
