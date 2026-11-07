module Ex.Tintinnabuli where

import Control.Monad (void)
import Control.Lens hiding ((:>))
import DenoMusic.Play
import FRP
import Music.Extra
import Music.Harmony
import Music.Types
import DenoMusic.Notation


dropMod :: Note s -> Note s
dropMod (Note n []) = Note n []
dropMod (Note n (x:xs)) =
  case compare x 0 of
    LT -> Note (n - 1) []
    EQ -> dropMod $ Note n xs
    GT -> Note n []


tintinnabuli :: Scale c s -> Note c -> Note s -> Note s
tintinnabuli sc c = scaleIso (opScale sc) %~ (+ c) . dropMod


melody :: Horizontal r (Note 7) ()
melody = foldMap (hit 0.25) $ fmap fromInteger [-12 .. 12]

harmonized :: Horizontal r (Vertical 2 (Note 7)) ()
harmonized = flip mapOut melody $ \m -> do
  let t = tintinnabuli triad 2 m
  m :> t :> Nil


song :: Signal (Event (Span (Vertical 2 (Note 7))))
song = horizontal harmonized


main :: IO ()
main = do
  toPdf song
  play song

