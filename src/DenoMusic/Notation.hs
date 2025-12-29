{-# OPTIONS_GHC -fno-warn-deprecations #-}

module DenoMusic.Notation
  ( toLilypond
  , toPdf
  ) where

import Data.Map.Monoidal qualified as MM
import Data.Foldable
import Data.Set (Set)
import DenoMusic.Types
import DenoMusic.NotationBackend (finalizeLily, header, footer)
import System.Cmd (rawSystem)


toNotationVoices
  :: (Foldable t)
  => Music v (t c)
  -> [[(Interval Rational, Either a ([b], c))]]
toNotationVoices (Music _ m) = do
  (_, z) <- MM.toList m
  pure $ do
    (int, tc) <- flatten z
    -- TODO(sandy): This splits up the @cs@, but it just gets built back again
    -- in 'finalizeLily', and probably done poorly.
    c <- toList tc
    pure (int, Right ([], c))


toLilypond :: Music v (Set (Reg PitchClass)) -> String
toLilypond = finalizeLily . toNotationVoices


-- | Generate lilypond code for a 'Music', and dump the resulting pdf to
-- @\/tmp\/song.pdf@.
toPdf :: Music v (Set (Reg PitchClass)) -> IO ()
toPdf m = do
  let lp = toLilypond m
  writeFile "/tmp/out.lily" $ header <> lp <> footer
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()

