
module Pieces.Motive where

import Notation
import Theory.Chords
import Combinators
import Score
import Euterpea (Pitch, PitchClass(..))
import Euterpea (pitch, absPitch, trans)

dot = scale 1.5

m1 = mconcat
  [ tile (1/8) (F, 4)
  , tile (1/8) (C, 4)
  , dot $ tile (1/2) (Af, 4)
  ]

m2 = mconcat
  [ tile (1/8) (C, 4)
  , tile (1/8) (Ef, 4)
  , delay (1/8)
  , tile (1/8) (C, 4)
  , tile (1/8) (G, 4)
  , delay (1/8)
  ]


m3 = mconcat
  [ delay (1/8)
  , tile (1/8) (Ef, 4)
  , tile (1/4) (D, 4)
  ]


dope = mconcat
  [ m1
  , fmap (trans 5) m1
  , fmap (trans 7) $ negativeHarmony' (C, 4) $ m2 <> m3
  ]

redouble p = tile 0.25 p <> tile 0.25 p <> tile 0.25 p <> tile 0.25 p

score :: Score Pitch
score =
  flip fork (note (F, 2) <> note (Bf, 2) <> note (Ef, 2) <> (note (Gs, 2) >>= redouble) <> note (Gs, 2) <> note (Bf, 2) <> tile (6/8) (G, 2) <> tile (1/8) (C, 2) <> tile (1/4) (F, 2)) $
  mconcat
    [ m1
    , fmap (trans 5) m1
    , fmap (trans 10) $ negativeHarmony' (C, 4) m2
    , fmap (trans 8) $ negativeHarmony' (C, 4) m1
    , delay 0.125
    , dope
    ]

main :: IO ()
main = do
  toPdf score
  playScore score
