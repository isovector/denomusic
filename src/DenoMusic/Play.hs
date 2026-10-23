{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module DenoMusic.Play (play) where

import Control.Lens (_1, _2, _3, view)
import FRP.Primitives
import Control.DeepSeq (NFData)
import Data.Foldable
import Data.Set (Set)
import Euterpea qualified as E
import FRP.Types
import FRP (stretch)
import Music.Harmony
import Music.Types

toMusicF :: (E.ToMusic1 b) => (a -> b) -> E.Music a -> E.Music1
toMusicF f = E.toMusic1 . fmap f

bindMusic :: (E.Dur -> a -> E.Music b) -> E.Music a -> E.Music b
bindMusic f =
  E.mFold
    (\case
      E.Note dur a -> f dur a
      E.Rest dur -> E.rest dur
    )
    (E.:+:)
    (E.:=:)
    E.Modify

instance {-# OVERLAPPING #-} E.ToMusic1 a => E.ToMusic1 (Interval Time, a) where
  toMusic1 = bindMusic $ \dur (Interval lo hi, a) ->
    E.rest (unTime lo) E.:+: E.toMusic1 (E.note (unTime (hi - lo) * dur) a)

instance E.ToMusic1 a => E.ToMusic1 (Timed a) where
  toMusic1 = bindMusic $ \dur (Timed t a) ->
    E.toMusic1 $ E.note (dur * unTime t) a

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
  toMusic1 = bindMusic $ \dur v ->
    case fmap (\a -> E.toMusic1 (E.Prim (E.Note dur a))) v of
      [] -> E.Prim $ E.Rest 0
      ms -> foldr1 (E.:=:) ms


instance {-# OVERLAPPABLE #-} (E.ToMusic1 a, E.ToMusic1 b) => E.ToMusic1 (a, b) where
  toMusic1 = bindMusic $ \dur (a, b) ->
    E.toMusic1 (E.note dur a) E.:=: E.toMusic1 (E.note dur b)

instance (KnownNat v, E.ToMusic1 a) => E.ToMusic1 (Vertical v a) where
  toMusic1 = toMusicF toList

instance E.ToMusic1 o => E.ToMusic1 (SF () (Event o)) where
  toMusic1 = bindMusic $ \dur v ->
    let go _ [] = E.rest 0
        go now ((t, a) : es) =
          let onset = unTime t
              m     = E.toMusic1 (E.note dur a)
           in (E.rest (onset - now) E.:+: m) E.:+: go (onset + E.dur m) es
     in go 0 $ observe $ stretch dur v

instance (E.ToMusic1 (SF i a), E.ToMusic1 (SF i b)) => E.ToMusic1 (SF i (a, b)) where
  toMusic1 = bindMusic $ \dur sf ->
    E.toMusic1 (E.note dur $ fmap fst sf) E.:=: E.toMusic1 (E.note dur $ fmap snd sf)

instance (E.ToMusic1 (SF i a), E.ToMusic1 (SF i b), E.ToMusic1 (SF i c)) => E.ToMusic1 (SF i (a, b, c)) where
  toMusic1 = bindMusic $ \dur sf ->
    E.toMusic1 (E.note dur $ fmap (view _1) sf)
      E.:=: E.toMusic1 (E.note dur $ fmap (view _2) sf)
      E.:=: E.toMusic1 (E.note dur $ fmap (view _3) sf)


deriving anyclass instance NFData (Deg n)
deriving anyclass instance NFData (Note n)
deriving anyclass instance NFData a => NFData (Timed a)
deriving anyclass instance NFData Time


-- | Play a piece of music by converting it to MIDI.
play :: (E.ToMusic1 a) => a -> IO ()
play = E.playDev 2 . E.toMusic1 . E.note 1

