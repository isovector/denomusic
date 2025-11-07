{-# OPTIONS_GHC -fno-warn-x-partial #-}

module Theory.Chords where

import Euterpea (PitchClass(..), Pitch, trans)

maj :: PitchClass -> Int -> [Pitch]
maj = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  ]

power :: PitchClass -> Int -> [Pitch]
power = curry $ \r ->
  [ r
  , trans 7 r
  , trans 12 r
  ]

minor :: PitchClass -> Int -> [Pitch]
minor = curry $ \r ->
  [ r
  , trans 3 r
  , trans 7 r
  ]

dim :: PitchClass -> Int -> [Pitch]
dim = curry $ \r ->
  [ r
  , trans 3 r
  , trans 6 r
  ]

maj7 :: PitchClass -> Int -> [Pitch]
maj7 = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  , trans 11 r
  ]

min7 :: PitchClass -> Int -> [Pitch]
min7 = curry $ \r ->
  [ r
  , trans 3 r
  , trans 7 r
  , trans 10 r
  ]

dom7 :: PitchClass -> Int -> [Pitch]
dom7 = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  , trans 10 r
  ]

invert :: [Pitch] -> [Pitch]
invert [] = []
invert ((pc, o) : ps) = ps <> [(pc, o + 1)]

inversion :: Int -> [Pitch] -> [Pitch]
inversion n = head . drop n . iterate invert
