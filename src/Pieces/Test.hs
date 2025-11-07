{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Pieces.Test where

import Data.Foldable
import Data.Sequence (Seq((:|>)))
import Data.Sequence qualified as Seq
import Data.Ratio
import Theory.Chords
import Tile
import Euterpea (PitchClass (..), Pitch, trans)
import Data.Semigroup

sec1 :: Tile Pitch
sec1 = scaleTo 1 $ stimes 4 $ foldMap (tile 1) $ minor B 3

sec2 :: Tile Pitch
sec2 = fmap (trans (-12)) sec1


chord :: [a] -> Tile a
chord = simul . fmap (tile 1)


octaveChord :: PitchClass -> Int -> Tile Pitch
octaveChord pc o =
  chord
      [ (pc, o)
      , (pc, o + 1)
      ]

score :: Seq (Tile Pitch)
score = Seq.fromList
  [ sec1
  , sec1
  , sec2
  , sec2
  , re (tile 1 (B, 2)) <> sec1
  , re (tile 1 (B, 1)) <> sec1
  , re (tile 1 (B, 2)) <> sec1
  , re (chord [(B, 2), (Fs, 3)]) <> sec1
  , re (anticipate $ chord [(B, 2), (Fs, 3)]) <> sec1
  , re (tile 1 (B, 1)) <> sec1
  , re (tile 1 (B, 2)) <> sec1
  , re (tile 1 (A, 2)) <> sec1
  , re (anticipate $ tile 1 (G, 2)) <> sec1
  , re (tile 1 (Fs, 2)) <> sec1
  , re (tile 1 (B, 2)) <> sec1
  ]

anticipate :: Tile a -> Tile a
anticipate a = bind a $ \pc -> co (tile 1 pc) <> tile 9 pc

takeR :: Int -> Seq a -> Seq a
takeR _ Seq.Empty = Seq.Empty
takeR 0 _ = Seq.Empty
takeR n (xs :|> x) = takeR (n - 1) xs :|> x


main :: IO ()
main = playTile $ scale (3 % 2) $ fold $ takeR 3 score

