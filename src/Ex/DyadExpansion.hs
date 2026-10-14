module Ex.DyadExpansion where

import DenoMusic.Play
import FRP
import Music.Extra
import Music.Harmony
import Music.Types


dyad :: Scale 2 7
dyad = mkScale [0, 2]

holdAccum :: c -> SF (Event (c -> c)) c
holdAccum d0 = hold d0 <<< accum d0


song :: SF () (Event (Notes (Vertical Dyad (Note 7))))
song = proc _ -> do
  v <- holdAccum mempty
   <<< arr (fmap (<>))
   <<< discrete
         ( zip [id @Time 0, 0.5 ..]
         $ cycle [voiceLead @Dyad @7 0 3, expand Lo Hi]
         )
    -< ()
  e <- every 0.5 (id @Time 0.5, ()) -< ()
  returnA
    -< note (const $ applyVoiceLeading v d0) e
 where
  d0 = chord triad

main :: IO ()
main = play $ export (0, 12) song

