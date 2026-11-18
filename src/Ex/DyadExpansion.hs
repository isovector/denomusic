module Ex.DyadExpansion where

import DenoMusic.Notation
import DenoMusic.Play
import FRP
import Music.Extra
import Music.Harmony
import Music.Types
import Data.Semigroup


divergence :: Horizontal r i (VoiceLeading (Vertical 2) 7) ()
divergence = stimes 12 $ foldMap (hit 0.25)
  [ voiceLead 0 3
  , expand 0 1
  ]



song :: SF () (Event (Span (Vertical 2 (Note 7))))
song = proc _ -> do
  vl <- msumE <<< getHorizontal divergence
  b <- fmap (fmap getSpan) $ horizontal $ stimes 12 $ hit 0.25 ()
  let ns = applyVoiceLeading vl (chord triad)
  ns' <- arr fromV <<<
    [: n <- ns
     | clampOctave (-9, 0) 0.5 -< n <$ b
     , clampOctave (0, 7)  0.5 -< n <$ b
     :]
  returnA -< fmap (Span 0.5) ns'


main :: IO ()
main = do
  toPdf $ toV $ fmap (toV . fmap toV) song
  play song

