{-# LANGUAGE DerivingVia #-}

module Etude16 where

import Data.Ratio
import Control.Applicative
import Rhythm
import Euterpea (Pitch, PitchClass(..))
import Score

bassRhythm :: Rhythm a -> Rhythm a -> Rhythm a
bassRhythm a b = tuplet
  [ a, b, b
  , a, b, a, b
  ]

bassTemplate :: Pitch -> Pitch -> Pitch -> Rhythm Pitch
bassTemplate a b c =
  bassRhythm
    (chord [a, b])
    (pure c)

asLastBeat :: Rhythm a -> Rhythm a
asLastBeat a = weightedTuplet
  [ (5 % 7, empty)
  , (2 % 7, a)
  ]


score :: Score Pitch
score = mconcat
  [ bar $
      bassTemplate
        (D, 3)
        (G, 3)
        (Bf, 3)
  , bar $ asum
      [
        bassTemplate
          (D, 3)
          (G, 3)
          (Bf, 3)
      , asLastBeat $ im
          [ (G, 2)
          , (A, 2)
          , (G, 2)
          , (Fs, 2)
          ]
      ]
  ]

main :: IO ()
main = playScore score


