module Ex.Rhythm where

import Control.Monad
import DenoMusic.Notation
import DenoMusic.Play
import Music.Types.Rhythm
import Music.Types
import Music.Harmony
import FRP

routing :: SF () a -> Signal a
routing sf = runSF sf $ pure ()

vls :: Signal (Event (VoiceLeading (Vertical 3) 7))
vls = routing $ accum mempty <<< arr (fmap (<>)) <<<
  ( discrete
      [ (0, mempty)
      , (2, voiceLead 0 4)
      , (3, voiceLead 0 4)
      , (5, voiceLead 0 4)
      , (6, voiceLead 0 4)
      ]
  )

beats :: Signal (Event ())
beats = mop (\_ _ -> ((), rstack 3 <.> lfan 3)) vls

song :: Signal (Event (Span (Vertical 3 (Note 7))))
song = spanner $ routing $ proc _ -> do
  evl <- constS vls -< ()
  vl <- hold mempty -< evl
  e <- constS beats -< ()
  returnA -< applyVoiceLeading vl (chord triad) <$ (e <|> void evl)

spanner :: Signal (Event a) -> Signal (Event (Span a))
spanner = runSF $ proc ea -> do
  spanning -< (NoEvent, ea)

main :: IO ()
main = do
  toPdf defaultNotationOptions song
  play song

