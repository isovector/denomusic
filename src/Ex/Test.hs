{-# LANGUAGE DeriveAnyClass #-}

module Ex.Test where

import DenoMusic.Play
import FRP
import Music.Harmony
import Music.Types


dyad :: Scale 2 3
dyad = mkScale [0, 1]


song :: SF () (Event (Timed (Vertical Bool (Note 7))))
song = proc _ -> do
  t <- hold mempty <<< accum mempty <<< every 1 (<> lead @3 @7 0 4) -< ()
  b <- rhythm [1/3, 2/3] (<> lead @2 @3 0 2) -< ()
  d <- hold mempty <<< accum mempty -< b
  returnA -< flip fmap (1/3 <$ b) $ \x -> Timed x $ fmap (applyScale $ apply d dyad >>> apply t triad) $ iota @Bool


main :: IO ()
main = play $ export (0, 12) song


