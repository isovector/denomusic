{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DenoMusic.Play (play) where

import Control.DeepSeq (NFData)
import Data.Foldable
import Data.Set (Set)
import DenoMusic.Types
import Euterpea qualified as E
import FRP.Types.Time
import Music.Harmony
import Music.Types

toMusicF :: (E.ToMusic1 b) => (a -> b) -> E.Music a -> E.Music1
toMusicF f = E.toMusic1 . fmap f

instance E.ToMusic1 a => E.ToMusic1 (Timed a) where
  toMusic1 = toMusicF unTimed

instance E.ToMusic1 a => E.ToMusic1 (Set a) where
  toMusic1 = toMusicF toList

instance E.ToMusic1 (Deg 12) where
  toMusic1 = toMusicF $ (+ 60) . getDeg

instance E.ToMusic1 (Note 12) where
  toMusic1 = toMusicF noteDeg

instance E.ToMusic1 (Note 7) where
  toMusic1 = toMusicF $ applyScale diatonic

instance E.ToMusic1 (Note 3) where
  toMusic1 = toMusicF $ applyScale triad

instance E.ToMusic1 a => E.ToMusic1 [a] where
  toMusic1 =
    E.mFold
      (\case
        E.Note dur v ->
          foldr
            (\a m -> m E.:=: E.toMusic1 (E.Prim (E.Note dur a)))
            (E.Prim $ E.Rest 0)
            v
        E.Rest dur -> E.Prim $ E.Rest dur
      )
      (E.:+:)
      (E.:=:)
      E.Modify

instance (Enum v, Bounded v, E.ToMusic1 a) => E.ToMusic1 (Vertical v a) where
  toMusic1 = toMusicF toList

deriving anyclass instance NFData (Deg n)
deriving anyclass instance NFData (Note n)
deriving anyclass instance NFData a => NFData (Timed a)
deriving anyclass instance NFData Time


-- | Play a piece of music by converting it to MIDI.
play :: (E.ToMusic1 a, NFData a) => [(Interval Time, a)] -> IO ()
play z
  = E.playDev 2
  $ foldr (E.:=:) (E.rest 0)
  $ do
    (Interval (Time lo) (Time hi), a) <- z
    pure $ E.rest lo E.:+: E.note (hi - lo) a

