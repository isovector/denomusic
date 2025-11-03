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


twiddle :: Score Pitch
twiddle = foldr1 after
  [ asBars (2 % 7) $
      im
        [ (G, 2)
        , (A, 2)
        , (G, 2)
        , (Fs, 2)
        ]
  , asBars (10 % 7) $ pure (G, 2)
  ]

strike :: Score Pitch
strike = delay (- (1 % 7)) $ foldr1 after
  [ asBars (1 % 7) $ chord
      [ (Fs, 5)
      , (Fs, 6)
      ]
  , asBars (10 % 7) $ chord
      [ (G, 5)
      , (G, 6)
      ]
  ]

score :: Score Pitch
score = foldr1 after
  -- [ b
  -- [ b
  [ twiddle
  -- , parL b strike
  -- , b
  -- , parL b twiddle
  -- , b
  -- , parL b strike
  -- , b'
  -- , b''
  ]
  where
    b = bar $ bassTemplate (D, 3) (G, 3) (Bf, 3)
    b' = bar $ bassTemplate
          (F, 3)
          (G, 3)
          (Bf, 3)
    b'' = bar $ bassTemplate
          (Ef, 3)
          (G, 3)
          (Bf, 3)


main :: IO ()
main = playScore score


