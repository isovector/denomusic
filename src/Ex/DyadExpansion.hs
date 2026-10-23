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
divergence = resize 0.5 $ replicateH 12 $ foldMap dnote
  [ voiceLead 0 3
  , expand 0 1
  ]


song :: SF () (Event (Timed (Vertical 2 (Note 7))))
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

