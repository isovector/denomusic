module Ex.Rhythm where

import DenoMusic.Notation
import DenoMusic.Play
import Music.Types.Rhythm
import Music.Types
import Music.Harmony
import FRP

vls :: SF i (Event (VoiceLeading (Vertical 3) 7))
vls = accum mempty <<< arr (fmap (<>)) <<<
  ( discrete
      [ (0, mempty)
      , (2, voiceLead 0 4)
      , (3, voiceLead 0 4)
      , (5, voiceLead 0 4)
      , (6, voiceLead 0 4)
      ]
  )

song :: SF () (Event (Span (Vertical 3 (Note 7))))
song = proc _ -> do
  evl <- vls
  vl <- hold mempty -< evl
  e <- mop (\_ _ -> ((), rstack 3 <.> lfan 3)) -< evl
  returnA -< fmap (applyVoiceLeading vl (chord triad) <$) $ e <|> Span 1 () <$ evl

main :: IO ()
main = do
  toPdf defaultNotationOptions song
  play song

