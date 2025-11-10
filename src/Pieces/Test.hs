{-# LANGUAGE DerivingStrategies         #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Pieces.Test where

import Data.List (intersperse)
import Control.Concurrent.Async
import Notation
import Data.Foldable
import Data.Sequence (Seq((:|>)))
import Data.Sequence qualified as Seq
import Data.Ratio
import Theory.Chords
import Score
import Euterpea (PitchClass (..), Pitch, trans)
import Data.Semigroup

-- | B-aeolian
toneChord :: Int -> Int -> Int -> [Pitch]
toneChord = chordic D 6

line1 :: Score Pitch
line1 = scaleTo 1 $ foldMap note $ mconcat
  [ toneChord 1 5 2
  , toneChord 1 5 2
  , toneChord 1 3 2
  ]

line2 :: Score Pitch
line2 = scaleTo 1 $ foldMap note $ mconcat
  [ toneChord 1 5 2
  , toneChord 1 5 2
  , toneChord 1 3 3
  ]

octaveChord :: PitchClass -> Int -> Score Pitch
octaveChord pc o = scaleTo (3 / 4) $
  chord
      [ (pc, o)
      , (pc, o + 1)
      ]

sec1 :: Score Pitch
sec1 = mconcat
  [ line1
  , re (octaveChord B 4) <> line1
  , re (octaveChord A 4) <> line1
  , re (anticipate (1/8) $ octaveChord B 4) <> line2
  , fmap (trans 12) $ line1
  , re (octaveChord B 2) <> fmap (trans 12) line1
  , re (octaveChord A 2) <> fmap (trans 12) line1
  , re (anticipate (1/4) $ octaveChord G 2) <> fmap (trans 12) line1
  , re (scaleTo 1 $ octaveChord Fs 2 <> scale 3 (octaveChord G 2)) <> fmap (trans 12) line1
  ]

n2 = tile (1/2)
n3 = dot . tile (1/4)
n4 = tile (1/4)
n8 = tile (1/8)
dot = scale (3/2)


newtype Tone = Tone Int
  deriving newtype (Show, Eq, Enum, Num)

tone :: Tone -> Pitch
tone (Tone i) =
  let pcs = addAscendingOctave 3 $ fmap (addSemitonesToPitchClass D) $ cycle $ nthMode 6 ionianSemitones
   in pcs !! i


sec2 :: Score Pitch
sec2 = mconcat
  [ fork line1 $ scale (1/8) $ foldMap (tile 1) $ (Fs, 4) : intersperse (Fs, 4) [(B, 4), (Cs, 5), (D, 5), (E, 5)] ++ [(Fs, 4)]
  , fork (octaveChord A 2) $ v1 11
  , delay (1/8)
  , fork (octaveChord G 2) $ v1 10
  , v1 9
  , delay (1/4)
  , n4 $ tone 7
  , n2 (tone 4)
  ]
  where
    v1 n =
      phrase $ mconcat
        [ n8 (tone n)
        , n8 (tone $ n + 1)
        , n8 (tone n)
        , n3 (tone $ n - 1)
        ]


sec3 :: Score Pitch
sec3 = mconcat
  [ dot $ delay (1/4)
  , n3 (Fs, 4)
  , n8 (E, 4)
  , n8 (D, 4)
  , chord [(G, 2), (E, 3), (G, 3), (B, 3)]
  , n4 (B, 3)
  , n8 (Cs, 4)
  , n8 (D, 4)
  , chord [(A, 2), (E, 3), (A, 3), (Cs, 4)]
  ]

score :: Seq (Score Pitch)
score = Seq.fromList
  [ sec3
  -- , sec2
  ]

anticipate :: Rational -> Score a -> Score a
anticipate d a = co (scaleTo d a) <> a

takeR :: Int -> Seq a -> Seq a
takeR _ Seq.Empty = Seq.Empty
takeR 0 _ = Seq.Empty
takeR n (xs :|> x) = takeR (n - 1) xs :|> x



main :: IO ()
main = do
  toPdf $ fold score
  playScore $ scale (4/3) $ fold $ takeR 1 score

