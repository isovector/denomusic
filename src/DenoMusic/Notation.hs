{-# OPTIONS_GHC -fno-warn-deprecations #-}

module DenoMusic.Notation
  ( toLilypond
  , toPdf
  ) where

import Data.Foldable
import Data.Set (Set)
import DenoMusic.Types
import DenoMusic.NotationBackend (finalizeLily, header, footer)
import System.Cmd (rawSystem)


toNotationVoices
  :: (Enum v, Bounded v, Foldable t)
  => Music v (t c)
  -> [[(Interval Rational, Either a ([b], c))]]
toNotationVoices (Music _ m) = do
  v <- enumFromTo minBound maxBound
  pure $ do
    (int, tc) <- flatten $ m v
    -- TODO(sandy): This splits up the @cs@, but it just gets built back again
    -- in 'finalizeLily', and probably done poorly.
    c <- toList tc
    pure (int, Right ([], c))


toLilypond :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> String
toLilypond = finalizeLily . toNotationVoices


-- | Generate lilypond code for a 'Music', and dump the resulting pdf to
-- @\/tmp\/song.pdf@.
toPdf :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> IO ()
toPdf m = do
  let lp = toLilypond m
  writeFile "/tmp/out.lily" $ header <> lp <> footer
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()

