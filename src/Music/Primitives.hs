module Music.Primitives where

import Data.Group
import Data.Ratio
import Data.Set (Set)
import Data.Tree.DUAL
import Music.Types


note :: Rational -> T -> Music
note d = stretch d . Music . leaf mempty { ua_width = 1 }

move :: T -> Music
move t = Music $ leafU mempty { ua_motion = t }

rest :: Rational -> Music
rest d = Music $ leafU mempty { ua_width = d }


stretch :: Rational -> Music -> Music
stretch d = addEnv $ mempty { e_duration = d }

timeSignature :: Integer -> Integer -> Music -> Music
timeSignature num den = addAnn (TimeSignature num den) . stretch (num % den)

tempo :: Rational -> Integer -> Music -> Music
tempo ndur bpm = addAnn $ Tempo ndur bpm

phrase :: Music -> Music
phrase = addAnn Phrase

-- Note to self: also add this as an annotation, so that we can correctly
-- generate a key signature.
--
-- https://lilypond.org/doc/v2.24/Documentation/notation/displaying-pitches#key-signature
withScale :: Set PitchClass -> Music -> Music
withScale sc = addEnv $ mempty { e_scale = pure sc }


voice :: Int -> Music -> Music
voice t = addEnv $ mempty { e_voice = pure t }


reharmonize :: T -> Music -> Music
reharmonize t m = move t <> m <> move (invert t)


withChord
  :: Set (Reg PitchClass)
  -> Music
  -> Music
withChord t = addEnv $ mempty { e_chord = pure t }

