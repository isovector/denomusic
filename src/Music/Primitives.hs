module Music.Primitives where

import Data.Monoid
import Data.Set (Set)
import Data.Tree.DUAL
import Music.Types


note :: Rational -> T -> Music
note d = stretch d . Music . leaf (Sum 1)


rest :: Rational -> Music
rest d = Music $ leafU $ Sum d


stretch :: Rational -> Music -> Music
stretch d = addEnv $ mempty { e_duration = d }


withScale :: Set PitchClass -> Music -> Music
withScale sc = addEnv $ mempty { e_scale = pure sc }


withRoot :: Reg PitchClass -> Music -> Music
withRoot t = addEnv $ mempty { e_root = pure t }


voice :: Int -> Music -> Music
voice t = addEnv $ mempty { e_voice = pure t }


withChord
  :: Set Int
  -- ^ scale degrees off of the current root
  -> Music
  -> Music
withChord t = addEnv $ mempty { e_chord = pure t }

