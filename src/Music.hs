module Music
  ( module Music.Types
  , module Music.Primitives
  , module Music.Rhythm
  , module Music.Play
  , module Music.Notation
  , module Music.Utils
  , module Music
  , scaleTone
  , chordTone
  , semiTone
  , register
  , inversion
  , Group (..)
  ) where

import Data.Group
import Data.Foldable
import Data.List (inits)
import Music.Harmony hiding (move)
import Music.Notation
import Music.Play
import Music.Primitives
import Music.Rhythm
import Music.Utils
import Music.Types


chord :: Foldable t => Rational -> t T -> Music
chord d ts = simul $ fmap (note d) $ toList ts


modulate :: [T] -> [Music] -> Music
modulate ts = mconcat . zipWith reharmonize (fmap fold $ inits ts)

