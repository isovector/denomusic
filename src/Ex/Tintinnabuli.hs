module Ex.Tintinnabuli where

import GHC.TypeLits
import Data.IntMap qualified as IM
import DenoMusic.Play
import FRP
import Music.Harmony
import Music.Types
import DenoMusic.Notation


tintinnabuli :: forall c s. (KnownNat s, 1 <= c) => Scale c s -> Note c -> Note s -> Note s
tintinnabuli sc c = applyScale sc . (+ c) . dropAccidental @s . applyScale (invertScaleAbove sc)


melody :: Horizontal r i (Note 7) ()
melody = foldMap (hit 0.25) $ fmap fromInteger [-12 .. 12]

harmonized :: Horizontal r i (Vertical 2 (Note 7)) ()
harmonized = flip mapOut melody $ \m -> do
  let t = tintinnabuli triad 2 m
  m :> t :> Nil


song :: SF () (Event (Span (Vertical 2 (Note 7))))
song = horizontal harmonized


main :: IO ()
main = do
  toPdf song
  play song

