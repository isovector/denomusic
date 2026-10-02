{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DenoMusic.Play (play) where

import Control.DeepSeq (NFData)
import Data.Set (Set)
import Data.Set qualified as S
import DenoMusic.Harmony
import DenoMusic.Types
import Euterpea qualified as E

instance E.ToMusic1 (Deg 12) where
  toMusic1 = E.toMusic1 . E.mMap (\(Deg n) -> n)

instance E.ToMusic1 (Note 12) where
  toMusic1 = E.toMusic1 . E.mMap (\(Note n _) -> n)

instance E.ToMusic1 (Reg PitchClass) where
  toMusic1 = E.toMusic1 . E.mMap (fromReg . fmap toStupidEuterpeaPitchClass)

deriving anyclass instance NFData PitchClass
deriving anyclass instance NFData (Reg PitchClass)
deriving anyclass instance NFData (Deg n)
deriving anyclass instance NFData (Note n)


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
play :: (E.ToMusic1 a, NFData a) => [(Interval Rational, Set a)] -> IO ()
play z
  = E.playDev 2
  $ foldr (E.:=:) (E.rest 0)
  $ do
    (Interval lo hi, as) <- z
    pure $ foldr (E.:=:) (E.rest 0) $ do
      a <- S.toList as
      pure $ E.rest lo E.:+: E.note (hi - lo) a

