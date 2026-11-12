module Ex.Tintinnabuli where

import Control.Monad (void)
import Data.IntMap qualified as IM
import Control.Lens hiding ((:>))
import DenoMusic.Play
import FRP
import Music.Extra
import Music.Harmony
import Music.Types
import DenoMusic.Notation


dropMod :: Note s -> Note s
dropMod (Note n (Ornaments os)) =
  case IM.lookupMin os of
    Just (_, x) | x < 0 -> Note (n - 1) mempty
    _ -> Note n mempty


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
  toPdf defaultNotationOptions {no_timesignature = Nothing, no_staves = IntrinsicStaff 6} song
  -- play song

