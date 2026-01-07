module DenoMusic (
  -- * Core Types
  Music,
  Reg (..),
  PitchClass (..),

  -- * Using Music
  defaultMain,
  play,
  toPdf,
  duration,
  separate,
  split,

  -- * Musical Primitives
  note,
  rest,
  delay,

  -- * Combining Music
  (##),
  line,
  stretch,

  -- * Combinators
  varyEnd,
  repeatFor,
  rotate,
  alternating,
  stamp,
  Finite,

  -- * Composing Voices
  voice,
  everyone,
  withVoice,
  fromVoices,
  mapVoices,

  -- * Harmony
  VoicePurpose (..),
  HasPurpose (..),
  chordChange,
  keyChange,
  triad,
  diatonic,
  spelledFlat,
  spelledSharp,

  -- * Western Harmony
  vl3in7,
  vl7in12,
  standard,

  -- * Modes
  dorian,
  phrygian,
  lydian,
  mixolydian,
  aeolian,
  locrian,

  -- * Harmonic Concerns
  T (..),
  elim,
  kill,
  MetaScales (..),
  MetaScale,
  extend,
  sink,
  countervail,

  -- * Rhythms
  dotted,
  whole,
  half,
  third,
  quarter,
  sixth,
  eighth,
  sixteenth,
  waltz,
  vienneseWaltz,
  sonClave,
  bossaNova,
  tango,
  reggae,
  samba,
  shuffle,
  march,

  -- * Reexports
  Set,
  Profunctor (..),
  Group (..),
) where

import Data.Group
import Data.Profunctor
import Data.Set (Set)
import Data.Set qualified as S
import DenoMusic.Generators ()
import DenoMusic.Harmony
import DenoMusic.Modes
import DenoMusic.Notation
import DenoMusic.Play
import DenoMusic.Rhythms
import DenoMusic.Types
import DenoMusic.Utils

-- | Generate sheet music and play the given 'Music'.
defaultMain
  :: Finite v
  => Reg PitchClass
  -- ^ "Home" pitch
  -> Music v (T '[3, 7, 12])
  -> IO ()
defaultMain root m = do
  let score = fmap (S.singleton . elim standard root) m
  toPdf score
  play score
