{-# OPTIONS_GHC -fno-warn-deprecations #-}

module DenoMusic.Notation
  ( finalizeLily
  , makeScore
  , Score
  , NotationOptions (..)
  , Staves (..)
  , defaultNotationOptions
  , toPdf
  , toPdf'
  , toPdfMusic
  , ToNotes (..)
  ) where

import DenoMusic.ToNotes (ToNotes (..), toNotesMusic)
import Euterpea qualified as E
import DenoMusic.NotationBackend (NotationOptions (..), Staves (..), defaultNotationOptions, finalizeLily, header, preamble, footer, makeScore)
import Data.Lilypond (Score)
import Music.Types.Note qualified as N
import System.Cmd (rawSystem)


-- toLilypond :: Music v (Set (Reg PitchClass)) -> String
-- toLilypond = finalizeLily . makeScore . toNotationVoices

-- toLilypondScore :: Music v (Set (Reg PitchClass)) -> Score
-- toLilypondScore = makeScore . toNotationVoices

-- | Generate lilypond code for anything that can be flattened into notes of an
-- @n@-note scale, and dump the resulting pdf to @\/tmp\/song.pdf@.
toPdf' :: forall n a. ToNotes n a => NotationOptions n -> a -> IO ()
toPdf' opts = toPdfMusic opts . toNotesMusic @n

-- | Like 'toPdf'', but for music that is already flattened.
toPdfMusic :: NotationOptions n -> E.Music (N.Note n) -> IO ()
toPdfMusic opts m = do
  let lp = finalizeLily $ makeScore opts m
  writeFile "/tmp/out.lily" $ preamble opts <> header <> lp <> footer
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()


-- | Generate lilypond code for a 'Score', and dump the resulting pdf to
-- @\/tmp\/song.pdf@.
toPdf :: ToNotes 12 a => a -> IO ()
toPdf = toPdf' defaultNotationOptions
