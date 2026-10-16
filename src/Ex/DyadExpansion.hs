module Ex.DyadExpansion where

import DenoMusic.Play
import DenoMusic.Notation
import FRP
import Music.Harmony
import Music.Types
import Music.Extra


dyad :: Scale 2 7
dyad = mkScale [0, 2]

holdAccum :: c -> SF (Event (c -> c)) c
holdAccum d0 = hold d0 <<< accum d0


divergence :: Horizontal i (Event (VoiceLeading Dyad 7))
divergence = resize 0.5 $ replicateH 12 $ foldMap dnote
  [ voiceLead 0 3
  , expand Lo Hi
  ]


song :: SF () (Event (Timed (Vertical Dyad (Note 7))))
song = proc _ -> do
  v <- holdAccum mempty
   <<< arr (fmap (<>))
   <<< offset 0.5
   <<< getHorizontal divergence
    -< ()
  (| (horizontal . resize 0.5 . replicateH 12)
   (do
      ev <- with id -< v
      returnA -< fmap (\vl -> applyVoiceLeading vl $ chord triad) ev
   )|)


main :: IO ()
main = do
  toPdf $ toV $ fmap (toV . fmap toV) song
  play song

