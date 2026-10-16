{-# OPTIONS_GHC -fno-warn-deprecations #-}

module DenoMusic.Notation
  ( finalizeLily
  , makeScore
  , Score
  , toPdf
  ) where

import DenoMusic.Types (Reg(..), PitchClass(..))
import Euterpea qualified as E
import DenoMusic.NotationBackend (finalizeLily, header, footer, makeScore)
import Data.Lilypond (Score)
import System.Cmd (rawSystem)


-- toLilypond :: Music v (Set (Reg PitchClass)) -> String
-- toLilypond = finalizeLily . makeScore . toNotationVoices

-- toLilypondScore :: Music v (Set (Reg PitchClass)) -> Score
-- toLilypondScore = makeScore . toNotationVoices

fromStupidEuterpeaPitchClass :: E.PitchClass -> PitchClass
fromStupidEuterpeaPitchClass E.Af = Af
fromStupidEuterpeaPitchClass E.A = A
fromStupidEuterpeaPitchClass E.As = As
fromStupidEuterpeaPitchClass E.Bf = Bf
fromStupidEuterpeaPitchClass E.B = B
fromStupidEuterpeaPitchClass E.C = C
fromStupidEuterpeaPitchClass E.Cs = Cs
fromStupidEuterpeaPitchClass E.Df = Df
fromStupidEuterpeaPitchClass E.D = D
fromStupidEuterpeaPitchClass E.Ds = Ds
fromStupidEuterpeaPitchClass E.Ef = Ef
fromStupidEuterpeaPitchClass E.E = E
fromStupidEuterpeaPitchClass E.F = F
fromStupidEuterpeaPitchClass E.Fs = Fs
fromStupidEuterpeaPitchClass E.Gf = Gf
fromStupidEuterpeaPitchClass E.G = G
fromStupidEuterpeaPitchClass E.Gs = Gs
fromStupidEuterpeaPitchClass _ = error "stupid"

-- | Generate lilypond code for a 'Score', and dump the resulting pdf to
-- @\/tmp\/song.pdf@.
toPdf :: E.ToMusic1 a => a -> IO ()
toPdf m = do
  let lp = finalizeLily $ makeScore $ fmap ((\(pc, o) -> Reg o $ fromStupidEuterpeaPitchClass pc) . fst) $ E.toMusic1 $ E.note 1 m
  writeFile "/tmp/out.lily" $ header <> lp <> footer
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()

