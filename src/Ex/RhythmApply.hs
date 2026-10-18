module Ex.RhythmApply where

import DenoMusic.Play
import DenoMusic.Notation
import FRP
import Music.Types


song :: Horizontal () (Event ((Note 7)))
song = proc _ -> do
  d <- holdH 0 <<< foldMap (hit 1) [1/3, 1 , 1/4 , 1/2] -< ()
  replicateH 10 hitDyn -< (d, fromIntegral $ floor $ recip $ unTime d)

holdH :: a -> Horizontal (Event a) a
holdH a0 = Horizontal $ proc i -> do
  a <- hold a0 -< i
  returnA -< (a, NoEvent)


main :: IO ()
main = do
  let s = horizontal song
  toPdf s
  play s


