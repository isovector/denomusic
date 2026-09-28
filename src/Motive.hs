{-# LANGUAGE OverloadedLists #-}

module Motive where

import DenoMusic.Play qualified as Play
import Control.Monad
import Data.Set qualified as S
import Data.Set (Set)
import FRP
import DenoMusic.Harmony
import DenoMusic.Types

hhit :: Time -> a -> Seq i (Event (Notes a)) ()
hhit t a = hit t $ Notes $ S.singleton (t, a)

nmap :: Ord b => (a -> b) -> Notes a -> Notes b
nmap f (Notes n) = Notes $ S.map (fmap f) n

m1 :: Seq i (Event (Notes (T [x, 7, 12]))) ()
m1 = do
  hhit (6/8) [0, 0, 0]
  hhit (1/8) [-1, 0, 0]
  hhit (1/8) [-2, 0, 0]
  -- change
  hhit (6/8) [0, 0, 0]
  rest (2/8)


m2 :: Seq i (Event (Notes (T [x, 7, 12]))) ()
m2 = do
  hhit (6/8) [0, 0, 0]
  hhit (1/8) [-2, 2, 0]
  hhit (1/8) [-2, 1, 0]
  hhit (6/8) [-2, 0, 0]
  rest (2/8)


m3 :: Seq i (Event (Notes (T [x, 7, 12]))) ()
m3 = do
  -- V
  hhit (1/8) [2, 0, 0]
  hhit (1/8) [1, 1, 0]
  hhit (1/8) [1, 0, 0]
  hhit (1/8) [0, 0, 0]
  -- I
  hhit (6/8) [0, 0, 0]
  rest (2/8)


a1 :: Seq i (Event (Notes (T [x, 7, 12]))) ()
a1 = do
  let h = hhit (1/8)
  h [0, 0, 0]
  h [1, 0, 0]
  h [3, 0, 0]
  h [1, 0, 0]
  h [2, 0, 0]
  h [2, 1, 0]
  h [2, 0, 0]
  h [1, 0, 0]


mm1 :: SF i (Event (Notes (T [x, 7, 12])))
mm1 = proc i -> do
  lh <- fmap (fmap $ nmap (<> [0, 0, -12])) $ getSeq (replicateM 6 a1) -< i
  rh <- fmap (fmap $ nmap (<> [0, 0, 12]))  $ getSeq (m1 >> m2 >> m3)  -< i
  returnA -< mconcat [lh, rh]


song :: SF i (Event (Notes (Reg PitchClass)))
song = mm1 >>> arr (fmap $ nmap $ elim (MSCons (add7 triad) $ MSCons diatonic spelledSharp) $ Reg 4 C)


main :: IO ()
main = do
  let ns = export (0, 8) song
  Play.play ns

