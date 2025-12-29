module DenoMusic.Play (play) where

import Data.Map.Monoidal qualified as MM
import Control.Arrow
import Data.Set (Set)
import Data.Set qualified as S
import DenoMusic.Types
import Euterpea qualified as E


toStupidEuterpeaPitchClass :: PitchClass -> E.PitchClass
toStupidEuterpeaPitchClass Af = E.Af
toStupidEuterpeaPitchClass A = E.A
toStupidEuterpeaPitchClass As = E.As
toStupidEuterpeaPitchClass Bf = E.Bf
toStupidEuterpeaPitchClass B = E.B
toStupidEuterpeaPitchClass C = E.C
toStupidEuterpeaPitchClass Cs = E.Cs
toStupidEuterpeaPitchClass Df = E.Df
toStupidEuterpeaPitchClass D = E.D
toStupidEuterpeaPitchClass Ds = E.Ds
toStupidEuterpeaPitchClass Ef = E.Ef
toStupidEuterpeaPitchClass E = E.E
toStupidEuterpeaPitchClass F = E.F
toStupidEuterpeaPitchClass Fs = E.Fs
toStupidEuterpeaPitchClass Gf = E.Gf
toStupidEuterpeaPitchClass G = E.G
toStupidEuterpeaPitchClass Gs = E.Gs


-- | Play a piece of music by converting it to MIDI.
play :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> IO ()
play (Music _ m)
  = E.playDev 2
  . fmap (first toStupidEuterpeaPitchClass)
  $ foldr (E.:=:) (E.rest 0)
  $ do
    (v, z) <- MM.toList m
    (Interval lo hi, as) <- flatten z
    pure $ foldr (E.:=:) (E.rest 0) $ do
      a <- S.toList as
      pure $ E.rest lo E.:+: E.note (hi - lo) (fromReg a)

