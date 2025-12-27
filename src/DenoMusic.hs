module DenoMusic
  ( -- * Core Types
    Music
  , Reg (..)
  , PitchClass (..)

  -- * Using Music
  , play
  , toPdf
  , duration

  -- * Musical Primitives
  , note
  , rest
  , delay

  -- * Combining Music
  , (##)
  , line
  , stretch

  -- * Composing Voices
  , voice
  , everyone
  , withVoice
  , fromVoices

  -- * Western Harmony
  , vl3in7
  , vl7in12
  , standard
  , triad
  , diatonic
  , spelledFlat
  , spelledSharp

  -- * Harmonic Concerns
  , T (..)
  , elim
  , MetaScales
  , MetaScale
  , extend
  , sink

  -- * Reexports
  , Set
  , Profunctor(..)
  , Group (..)
  ) where

import Data.Group
import DenoMusic.Types
import DenoMusic.Harmony
import DenoMusic.Utils
import DenoMusic.Play
import DenoMusic.Notation
import Data.Set
import Data.Profunctor
import DenoMusic.Generators ()
