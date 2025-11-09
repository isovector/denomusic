{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Pieces.Test where

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

sec1 :: Score Pitch
sec1 = scale (1/8) $ mconcat
  [ stimes 2 $ foldMap (tile 1) $ minor B 4
  , foldMap (tile 1) $ reverse $ take 2 $ minor B 1
  ]

sec2 :: Score Pitch
sec2 = fmap (trans (-12)) sec1


octaveChord :: PitchClass -> Int -> Score Pitch
octaveChord pc o =
  chord
      [ (pc, o)
      , (pc, o + 1)
      ]

score :: Seq (Score Pitch)
score = Seq.fromList
  [ sec1
  , sec1
  , sec2
  , sec2
  -- , re (tile 1 (B, 2)) <> sec1
  -- , re (tile 1 (B, 1)) <> sec1
  -- , re (tile 1 (B, 2)) <> sec1
  -- , re (chord [(B, 2), (Fs, 3)]) <> sec1
  -- , re (anticipate $ chord [(B, 2), (Fs, 3)]) <> sec1
  -- , re (tile 1 (B, 1)) <> sec1
  -- , re (tile 1 (B, 2)) <> sec1
  -- , re (tile 1 (A, 2)) <> sec1
  -- , re (anticipate $ tile 1 (G, 2)) <> sec1
  -- , re (tile 1 (Fs, 2)) <> sec1
  -- , re (tile 1 (B, 2)) <> sec1
  ]

anticipate :: Score a -> Score a
anticipate a = co (scale (1/2) a) <> a

takeR :: Int -> Seq a -> Seq a
takeR _ Seq.Empty = Seq.Empty
takeR 0 _ = Seq.Empty
takeR n (xs :|> x) = takeR (n - 1) xs :|> x



main :: IO ()
main = do
  async $ toPdf $ fold score
  playScore $ fold score

