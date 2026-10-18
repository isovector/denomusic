module Ex.RhythmApply where

import DenoMusic.Play
import DenoMusic.Notation
import FRP
import Music.Types

times :: Horizontal i (Event Time)
times = foldMap (hit 1)
  [ 1/3
  , 1
  , 1/4
  , 1/2
  ]


song :: SF () (Event (Timed (Note 7)))
song = proc _ -> do
  d <- hold 1
   <<< getHorizontal times
    -< ()
  (| (horizontal . replicateH 10)
      ( do
          hitDyn -< (d, fromIntegral $ floor $ recip $ unTime d)
      )
   |)


main :: IO ()
main = do
  toPdf song
  play song


