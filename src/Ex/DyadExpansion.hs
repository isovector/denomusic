module Ex.DyadExpansion where

import Data.Set qualified as S
import DenoMusic.Notation
import DenoMusic.Play
import FRP
import Music.Extra
import Music.Harmony
import Music.Types


dyad :: Scale 2 7
dyad = mkScale $ S.fromList [0, 2]

holdAccum :: c -> SF (Event (c -> c)) c
holdAccum d0 = hold d0 <<< accum d0


divergence :: Horizontal i (Event (VoiceLeading (Vertical 2) 7))
divergence = replicateH 12 $ foldMap dnote
  [ voiceLead 0 3
  , expand 0 1
  ]


song :: SF () (Event (Timed (Vertical 2 (Note 7))))
song = horizontal $ resize 0.5 $ proc i -> do
  vl <- liftH msumE <<< divergence -< i
  ns <- replicateH 12 $ with id -< applyVoiceLeading vl $ chord triad
  returnA -< ns


main :: IO ()
main = do
  toPdf $ toV $ fmap (toV . fmap toV) song
  play song

