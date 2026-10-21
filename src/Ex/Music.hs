module Ex.Music where

import Data.Set qualified as S
import DenoMusic.Notation
import DenoMusic.Play
import FRP
import Music.Harmony
import Music.Types
import Data.Coerce


dyad :: Scale 2 3
dyad = mkScale $ S.fromList [0, 1]

gesualdo :: VoiceLeading (Vertical Triad) 12
gesualdo = scaleSteps (-1) <> neoriemannian Root Third

tr :: Vertical Triad (Note 12)
tr = Vertical $ \case
  Root -> 0
  Third -> 4
  Fifth -> 7

dd :: Vertical Dyad (Note 3)
dd = Vertical $ \case
  Lo -> -2
  Hi -> -1

song :: SF () (Event (Timed (Note 12)), Event (Timed (Note 12)), Event (Timed (Vertical Dyad (Note 12))))
song = proc _ -> do
  v <-
    (| (horizontal (mconcat $ take 10 $ do
      vl <- iterate (<> gesualdo) $ scaleSteps 12
      pure $ hit 1 $ applyVoiceLeading vl tr
      )) |)
  dy <-
    (| (horizontal (mconcat $ take 15 $ do
      (vl, d) <- zip (scanl (<>) mempty $ cycle [voiceLead 0 1, voiceLead 0 1, chordalSteps 1]) $ cycle [0.5, 0.25, 0.25]
      pure $ hit d $ applyVoiceLeading vl dd
      )) |)
  vc <- hold tr -< fmap unTimed v
  s <- horizontal $ accumH 0 <<< something -< ()
  m <- horizontal motive -< ()
  returnA -< intrinsic vc $ \_ sc _ ->
    ( fmap (fmap $ applyScale sc . (+ 3) . coerce) s
    , fmap (fmap $ applyScale sc . (+ 3) . coerce) m
    , fmap (fmap $ fmap $ applyScale sc . coerce) dy
    )

motive :: Horizontal i (Event (Note s))
motive = mconcat
  [ hit 1 3
  , hit 1 3
  , hit 1 3
  , hit 1 3
  ]

something :: Horizontal i (Event (Note s -> Note s))
something = fmap (fmap (+)) $ replicateH 3 $ mconcat
  [ hit 0.25 0
  , hit 0.125 1
  , hit 0.125 (0 /+ 1)
  , hit 0.125 (1 /- 1)
  , hit 0.25 (-2)
  , hit 0.125 (-1)
  --
  , hit 0.125 0
  , hit 0.125 (0 /- 1)
  , hit 0.125 (-1 /+ 1)
  , hit 0.125 2
  , rest 0.25
  , rest 0.125
  ]

main :: IO ()
main = do
  toPdf song
  play song



