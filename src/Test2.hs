{-# LANGUAGE OverloadedLists #-}

module Test2 where

import Data.Semigroup
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
import FRP.Extra

-- need a way to duplicate 'discrete' over time
-- need better ways of describing non-regular rhythm

chords :: SF x (Event (Chord 4 [7, 12]))
chords = discrete $ zip [0, (2/3) ..] $ fmap (add7 triad,) $
  pat
    [ [1, -2, 0]
    , [3, -6, 0]
    , [2, -4, 0]
    ]

pat :: Monoid a => [a] -> [a]
pat = scanl (<>) mempty . cycle

innerVoice :: [C]
innerVoice = pat $ fmap (\x -> [x, 0, 0]) [1, -1, 1, 1]

note :: (Time, T [7, 12]) -> Notes (Reg PitchClass)
note = toNote (MSCons diatonic spelledSharp) mempty

-- ludwig :: SF Time (Chord 4 [7, 12])
-- ludwig = proc time -> do
--   let c1 :: Int = floor time
--       c2 :: Int = - c1
--       c3 = c1 + 2
--       c4 = c2 - 2
--   returnA -<
--     ( UnsafeMetaScale (S.fromList $ fmap (subtract c2) [c1, c2, c3, c4])
--     , [ 0, c2, 0 ]
--     )

song :: SF () (Event (Notes (Reg PitchClass)))
song = proc _ -> do
  ch <- hold undefined <<< chords -< ()
  chordTime <- every 0.25 (Beat (1/4) (P 0)) -< ()

  beats <- beatsOf time4'4 -< ()
  (strong, weak) <- partitionBeats (P 1) -< beats
  delayedWeak <- offset (1/8) -< weak
  earlyStrong <- offset (-1/8) -< strong


  t1 <- offset (-1/16) <<< every (1.5) (Beat (1/4) (P 0)) -< ()
  t2 <- offset (1/16) -< t1
  t3 <- offset (1/16) -< t2
  t4 <- offset (1/16) -< t3
  let rtimes = fmap getFirst $ sconcat $ fmap (fmap First) [t1, t2, t3, t4]



  ct <- line note (cycle $ fmap (\x -> [0, x, 0]) [0..]) -< (ch, delayedWeak)
  b1 <- line note (cycle $ fmap (\x -> [-x, 0, 0]) [0..]) -< (ch, strong)
  b2 <- line note (cycle $ fmap (\x -> [-x - 1, 0, 0]) [0..]) -< (ch, earlyStrong)

  iv <- line note (cycle innerVoice) -< (ch, rtimes)

  -- cho <- chord note -< (ch, chordTime)

  returnA -< mconcat
    [ b1
    , b2
    , ct
    , iv
    ]
  where
    rate = 1

-- --------------------------------------------------------------------------------


type C = T [4, 7, 12]



main :: IO ()
main = do
  let ns = export (0, 20) song
  -- toPdf $ makeScore $ pure $ fmap (\(i, s) -> (i, Right (mempty, S.findMin s))) ns
  Play.play ns

