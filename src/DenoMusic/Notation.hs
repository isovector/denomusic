{-# OPTIONS_GHC -fno-warn-deprecations #-}

module DenoMusic.Notation
  ( finalizeLily
  , makeScore
  , Score
  , toPdf
  ) where

import DenoMusic.NotationBackend (finalizeLily, header, footer, makeScore)
import Data.Lilypond (Score)
import System.Cmd (rawSystem)


-- toLilypond :: Music v (Set (Reg PitchClass)) -> String
-- toLilypond = finalizeLily . makeScore . toNotationVoices

-- toLilypondScore :: Music v (Set (Reg PitchClass)) -> Score
-- toLilypondScore = makeScore . toNotationVoices


-- | Generate lilypond code for a 'Score', and dump the resulting pdf to
-- @\/tmp\/song.pdf@.
toPdf :: Score -> IO ()
toPdf m = do
  let lp = finalizeLily m
  writeFile "/tmp/out.lily" $ header <> lp <> footer
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()

