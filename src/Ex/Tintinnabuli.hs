module Ex.Tintinnabuli where

import Control.Lens
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


melody :: Horizontal i (Event (Note 7))
melody = resize 0.25 $ foldMap dnote $
  fmap fromInteger [-12 .. 12]

harmonized :: Horizontal i (Vertical Bool (Event (Note 7)))
harmonized = proc i -> do
  m <- melody -< i
  let t = fmap (tintinnabuli triad 2) m
  returnA -<
    Vertical $ \case
      False -> m
      True -> t



-- song :: Vertical Bool (SF () (Event (Timed (Note 7))))
-- song = fmap horizontal $ toV $ harmonized

song :: SF () (Event (Timed (Vertical Bool (Note 7))))
song = horizontal $ fmap fromV harmonized


main :: IO ()
main = do
  toPdf song
  play song

