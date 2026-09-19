{-# LANGUAGE OverloadedLists #-}

module Test where

import Data.Coerce
import Data.Functor
import Data.Set qualified as S
import DenoMusic.Harmony
import DenoMusic.Modes
import DenoMusic.Notation
import DenoMusic.Play qualified as Play
import DenoMusic.Types
import FRP
import FRP.TimeSig

--------------------------------------------------------------------------------

chords :: SF x (Event Chord)
chords =
  discrete
    [ (0,   (add11  triad, [0,  0, 0]))
    , (1,   (coerce triad, [0, -1, 0]))
    , (1.5, (add11  triad, [0, -1, 0]))
    , (2,   (add11  triad, [0 , 0, 0]))
    , (3,   (coerce triad, [0, -1, 0]))
    , (3.5, (add11  triad, [0, -1, 0]))
    , (4,   (coerce triad, [0, -2, 0]))
    , (4.5, (add7   triad, [0, -2, 0]))
    , (6,   (coerce triad, [-1, 1, 0]))
    , (7,   (add7   triad, [-1, 2, 0]))
    ]

motif1 :: [C]
motif1 =
  [ [0, 0, 0]
  , [-1, 0, 0]
  , [-2, 0, 0]
  , [-2, 0, 0]
  , [-1, 0, 0]
  , [0, 0, 0]
  ]

bassline :: [C]
bassline =
  [ [-2, 0, -12]
  , [-2, 4, -12]
  ]



song :: SF () (Event (Notes (Reg PitchClass)))
song = proc _ -> do
  ch <- hold (add7 triad, mempty) <<< chords -< ()
  b  <- beatsOf (time4'4 >>= subdivide 2) -< ()
  sb <- filterEvents ((<= P 2) . stress) -< b
  wb <- filterEvents ((>  P 2) . stress) -< b

  (m1, _) <- replace (cycle motif1) -< wb
  (b1, _) <- replace (cycle bassline) -< sb

  m1' <- chordTone -< (ch, fmap (first duration) m1)
  b1' <- chordTone -< (ch, fmap (first $ const (1/4)) b1)

  returnA -< mconcat
    [ m1'
    , b1'
    ]

--------------------------------------------------------------------------------


type C = T [4, 7, 12]
type Chord = (MetaScale 4, C)


note :: SF (Event (Time, T [7, 12])) (Event (Notes (Reg PitchClass)))
note
  = arr
      (fmap $ \(t, x) -> Notes $ S.singleton (t, elim
          (MSCons harmonicMinor spelledSharp)
          (Reg 4 Fs)
          $ x <> mixolydian harmonicMinor)
      )

chordTone :: SF (Chord, Event (Time, C)) (Event (Notes (Reg PitchClass)))
chordTone = proc ((ms, t), e) ->
  note -< e <&> fmap (\t0 -> kill ms (t <> t0))



main :: IO ()
main = do
  let ns = export (0, 8) song
  toPdf $ makeScore $ pure $ fmap (\(i, s) -> (i, Right (mempty, S.findMin s))) ns
  Play.play ns

