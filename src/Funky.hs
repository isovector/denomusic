module Funky where

import Control.Monad.State
import Data.Bool
import Data.Ratio
import Etude17 (foldMusic, invert, renormalize)
import Euterpea (Pitch, PitchClass (..))
import Euterpea.IO.MIDI.Play
import Legacy hiding (main)
import Rhythm
import System.Random

main :: IO ()
main = do
  g <- initStdGen
  print g
  playDev @Pitch 2 $
    foldMusic $
      renormalize (1 % 8) $
        intervals $
          flip evalStateT g song

lydian :: PitchClass -> [PitchClass]
lydian pc =
  [ pc
  , addSemitonesToPitchClass pc 2
  , addSemitonesToPitchClass pc 4
  , addSemitonesToPitchClass pc 6
  , addSemitonesToPitchClass pc 7
  , addSemitonesToPitchClass pc 9
  , addSemitonesToPitchClass pc 11
  ]

rotate :: Int -> [a] -> [a]
rotate n as = drop n as <> take n as

ionian :: PitchClass -> [PitchClass]
ionian pc = rotate 4 $ lydian $ addSemitonesToPitchClass pc 5

mixolydian :: PitchClass -> [PitchClass]
mixolydian pc = rotate 4 $ ionian $ addSemitonesToPitchClass pc 5

dorian :: PitchClass -> [PitchClass]
dorian pc = rotate 4 $ mixolydian $ addSemitonesToPitchClass pc 5

aeolian :: PitchClass -> [PitchClass]
aeolian pc = rotate 4 $ dorian $ addSemitonesToPitchClass pc 5

phrygian :: PitchClass -> [PitchClass]
phrygian pc = rotate 4 $ aeolian $ addSemitonesToPitchClass pc 5

locrian :: PitchClass -> [PitchClass]
locrian pc = rotate 4 $ phrygian $ addSemitonesToPitchClass pc 5

randomT :: (Uniform a, Monad m) => StateT StdGen m a
randomT = state uniform

data Function = Tonic | Subdominant | Dominant

chorded :: Function -> [Int]
chorded Tonic = [1, 3, 5]
chorded Subdominant = [4, 6, 8]
chorded Dominant = [5, 7, 9]

song :: StateT StdGen Rhythm Pitch
song = do
  a <- lift $ im [Tonic, Subdominant, Dominant, Tonic]
  duplicate <- randomT @Bool
  b <- case duplicate of
    True -> lift $ im [a, a]
    False -> pure a
  let cs = chorded b
  arpegiate <- randomT @Bool
  c <- case arpegiate of
    True -> do
      c <- lift $ im cs
      motion <- randomT @Bool
      case motion of
        True -> do
          case c == last cs of
            True -> pure c
            False -> lift $ im [c, c + 1]
        False -> pure c
    False -> lift $ chord cs
  duplicate' <- randomT @Bool
  d <- case duplicate' of
    False -> pure c
    True -> do
      neighbor <- randomT @Bool
      case neighbor of
        True -> do
          up <- randomT @Bool
          lift $
            im
              [ c
              , bool (c - 1) (c + 1) up
              , c
              ]
        False -> do
          lift $ im [c, c]
  pure $ diatonicPitches !! (d + 6)

diatonicPitches :: [Pitch]
diatonicPitches = foldMap (take 1) $ iterate invert $ fmap (,3) $ ionian C
