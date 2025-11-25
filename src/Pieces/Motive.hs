{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Pieces.Motive where

import Notation hiding (toPitch)
import Data.Music.Lilypond.Pitch
import APitch
import Score

dot = scale 1.5

m1 = mconcat
  [ tile (1/8) fourth
  , tile (1/8) tonic
  , dot $ tile (1/2) sixth' -- flat?
  ]

m2 = mconcat
  [ tile (1/8) tonic
  , tile (1/8) third
  , delay (1/8)
  , tile (1/8) tonic
  , tile (1/8) fifth
  , delay (1/8)
  ]


m3 = mconcat
  [ delay (1/8)
  , tile (1/8) $ flat third
  , tile (1/4) second
  ]


dope = mconcat
  [ m1
  , fmap (fourth <>) m1
  , fmap (fifth <>) $ fmap inverted $ m2 <> m3
  ]

redouble p = tile 0.25 p <> tile 0.25 p <> tile 0.25 p <> tile 0.25 p

score :: Score APitch
score =
  -- flip fork (vb 2 fourth <> note (Bf, 2) <> note (Ef, 2) <> (note (Gs, 2) >>= redouble) <> note (Gs, 2) <> note (Bf, 2) <> tile (6/8) (G, 2) <> tile (1/8) (C, 2) <> tile (1/4) (F, 2)) $
  mconcat
    [ m1
    , fmap (fourth <>) m1
    , fmap (seventh <>) $ fmap inverted m2
    , fmap (sixth <>) $ fmap inverted m1
    , delay 0.125
    , dope
    ]

main :: IO ()
main = do
  -- toPdf $ fmap (toPitch naturalMinor (Pitch (C, 0, 4))) score
  playScore $ fmap (toPitch naturalMinor (Pitch (C, 0, 4))) score
