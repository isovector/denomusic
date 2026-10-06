module Ex.Horizontal where

import DenoMusic.Play
import FRP
import Music.Types
import DenoMusic.Harmony


m1 :: Note 12 -> H i (Note 12) (Note 12)
m1 s = do
  hit 0.25 $ s + 0
  hit 0.25 $ s + 1
  hit 0.25 $ s + 3
  pure $ s - 4

m2 :: Note 12 -> H i (Note 12) (Note 12)
m2 s = do
  let qq x = hit (0.125) $ s + x
  qq 0
  qq 12
  qq 10
  qq 8
  qq 7
  qq 10
  pure $ s + 10 - 1

song :: SF () (Event (Notes (Note 12)))
song = mconcat
  [ fmap (fmap timedToNotes . fmap (fmap (+ 60))) $
      getHorizontal $ m1 8     >>= m2 >>= m1 >>= m2 >>= m1
  , fmap (fmap timedToNotes . fmap (fmap (+ 60))) $
      getHorizontal $ m2 (-20) >>= m1 >>= m2 >>= m1 >>= m2
  ]

main :: IO ()
main = play $ export (0, 12) song
