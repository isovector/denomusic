{-# LANGUAGE DeriveAnyClass       #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module DenoMusic.Play (play, toAbsPitches) where

import Control.DeepSeq (NFData)
import DenoMusic.ToNotes
import Euterpea qualified as E
import FRP.Types
import Music.Types


-- | Flatten music into absolute MIDI pitches, where @0 :: Note 12@ is middle C.
-- Goes through the same 'ToNotes' instances used for notation.
toAbsPitches :: ToNotes 12 a => a -> E.Music E.AbsPitch
toAbsPitches = fmap ((+ 60) . getDeg . noteDeg) . toNotesMusic @12


deriving anyclass instance NFData (Deg n)
deriving anyclass instance NFData a => NFData (Accidentals a)
deriving anyclass instance NFData (Note n)
deriving anyclass instance NFData a => NFData (Span a)
deriving anyclass instance NFData Time


-- | Play a piece of music by converting it to MIDI.
play :: ToNotes 12 a => a -> IO ()
play = E.playDev 2 . toAbsPitches
