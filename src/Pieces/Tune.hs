{-# LANGUAGE OverloadedStrings #-}

module Pieces.Tune where

import Data.Semigroup
import Combinators
import Theory.Chords
import Notation
import Score
import Euterpea (Pitch, PitchClass(..))
import Euterpea (pitch, absPitch)

n :: PitchClass -> Int -> Score Pitch
n pc o = note (pc, o)

dot :: Score a -> Score a
dot = scale 1.5


theme :: Score Pitch
theme = voiced "melody" $ mconcat
  [ scale (1/2) $ n E 4
  , scale (1/4) $ mconcat
      [ n A 4
      , n C 5
      ]
  , dot $ scale (1/2) $ n B 4
  , delay (1/4)
  ]

counterpoint :: Score Pitch
counterpoint = scale 0.25 $ mconcat
  [ n A 4
  , n G 4
  , n F 4
  , n D 4
  , n E 4
  ]


score :: Score Pitch
score = mconcat
  [ fork (chord [(A, 2), (E, 3)]) theme
  , co $ scale 0.25 $ n F 4
  , fork (n F 2) $ fork (counterpoint) theme
  , delay (-0.25)
  , scale 0.25 $ n B 4
  ]



main :: IO ()
main = do
  let s' = score <> retro score
      s = do
            a@(_, o) <- s'
            case o < 4 of
              True -> scale (1/3) $ stimes 4 $ note a
              False -> pure a
  toPdf s
  playScore s

