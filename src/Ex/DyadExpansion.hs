module Ex.DyadExpansion where

import DenoMusic.Harmony
import DenoMusic.Play
import FRP
import Music.Extra
import Music.Types


dyad :: Scale 2 7
dyad = mkScale [0, 2]


song :: SF () (Event (Notes (Vertical Dyad (Note 12))))
song = proc _ -> do
  v <- hold d0
    <<< accum d0
    <<< notYet
    <<< every 1 (+ unenumerate [(Lo, -1), (Hi, 1)])
    -< ()
  t <- hold mempty
    <<< accum mempty
    <<< offset 0.5
    <<< every 1 (<> lead @2 @7 0 3)
    -< ()
  e <- every 0.5 (id @Time 0.5, ()) -< ()
  returnA
    -< note (const $ (+ 60) $ fmap (applyScale $ diatonic <<< apply t dyad) $ v) e
 where
  d0 = unenumerate [(Lo, 0), (Hi, 1)]

main :: IO ()
main = play $ export (0, 12) song

