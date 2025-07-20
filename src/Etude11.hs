{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Etude11 where

import Control.Monad
import Data.Ratio
import Etude17 (foldMusic, renormalize)
import Euterpea (Pitch, PitchClass (..))
import Euterpea.IO.MIDI.Play
import Legacy hiding (main)
import Rhythm

main :: IO ()
main =
  playDev @Pitch 2 $
    foldMusic $
      renormalize (1 % 8) $
        intervals song

chorded :: Rhythm [Pitch] -> Rhythm Pitch
chorded = (chord =<<)

brokenChord :: Rhythm [Pitch] -> Rhythm [Pitch]
brokenChord r = do
  ps <- r
  im [take 2 ps, drop 2 ps]

data HasLH = NoLH | QuarterLH | EightLH | StrikeLH

song :: Rhythm Pitch
song = tuplet $ do
  has_lh <-
    mconcat
      [ replicate 2 NoLH
      , replicate 4 QuarterLH
      , join $ replicate 4 [StrikeLH, EightLH]
      ]
  let rh = chorded $ brokenChord $ im $ replicate 6 $ minor C 5
  pure $
    Par rh $
      chorded $ case has_lh of
        NoLH -> Empty
        QuarterLH -> im $ replicate 4 $ minor C 4
        StrikeLH ->
          tuplet
            [ pure $ minor C 2
            , brokenChord $ pure $ minor C 4
            , brokenChord $ pure $ minor C 4
            , brokenChord $ pure $ minor C 4
            ]
        EightLH -> brokenChord $ im $ replicate 4 $ minor C 4
