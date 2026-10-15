module Ex.DyadExpansion where

import DenoMusic.Play
import FRP
import Music.Harmony
import Music.Types


dyad :: Scale 2 7
dyad = mkScale [0, 2]

holdAccum :: c -> SF (Event (c -> c)) c
holdAccum d0 = hold d0 <<< accum d0


divergence :: Horizontal i (VoiceLeading Dyad 7)
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
  (| (getHorizontal . resize 0.5 . replicateH 12)
   (do
      vl <- with unTimed -< v
      returnA -< applyVoiceLeading vl $ chord triad
   )|)


main :: IO ()
main = play $ export (0, 4) song

