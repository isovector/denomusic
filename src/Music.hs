module Music
  ( module Music.Types
  , module Music.Primitives
  , module Music.Rhythm
  , module Music.Play
  , module Music.Notation
  , module Music
  , scaleTone
  , chordTone
  , semiTone
  , register
  , inversion
  ) where

import Data.Foldable
import Data.List (inits)
import Music.Harmony
import Music.Notation
import Music.Play
import Music.Primitives
import Music.Rhythm
import Music.Types


reharmonize :: T -> Music -> Music
reharmonize t = addEnv $ mempty { e_harmony = t }


chord :: Foldable t => Rational -> t T -> Music
chord d ts = simul $ fmap (note d) $ toList ts


modulate :: [T] -> [Music] -> Music
modulate ts = mconcat . zipWith reharmonize (fmap fold $ inits ts)

