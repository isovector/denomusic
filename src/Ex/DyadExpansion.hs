module Ex.DyadExpansion where

import Data.Set qualified as S
import DenoMusic.Notation
import DenoMusic.Play
import FRP
import Music.Extra
import Music.Harmony
import Music.Types
import Data.Semigroup


divergence :: Horizontal r (VoiceLeading (Vertical 2) 7) ()
divergence = stimes 12 $ foldMap (hit 0.25)
  [ voiceLead 0 3
  , expand 0 1
  ]


routing :: SF () a -> Signal a
routing sf = runSF sf $ pure ()


song :: Signal (Event (Span (Vertical 2 (Note 7))))
song = routing $ proc i -> do
  vl <- msumE <<< constS (getHorizontal divergence) -< i
  b <- constS (fmap (fmap getSpan) $ horizontal $ stimes 12 $ hit 0.25 ()) -< ()
  let ns = applyVoiceLeading vl (chord triad)
  let (lo :> hi :> Nil) = ns
  ehi' <- clampOctave (0, 7) 0.5 -< hi <$ b
  elo' <- clampOctave (-9, 0) 0.5 -< lo <$ b
  returnA -< fmap (\(lo', hi') -> Span 0.25 $ lo' :> hi' :> Nil) $ liftA2 (,) elo' ehi'


main :: IO ()
main = do
  toPdf $ toV $ fmap (toV . fmap toV) song
  play song

