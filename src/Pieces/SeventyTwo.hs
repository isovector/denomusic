{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Pieces.SeventyTwo where

import Data.List (inits)
import Data.Set (Set)
import Notation2
import Music
import Data.Semigroup
import Data.Foldable
import qualified Data.Set as S
import Data.Functor
import MadMusic
import Euterpea (PitchClass(..))

sc :: Set PitchClass
sc = S.fromList [A, B, C, D, E, F, G]

motif1 :: Music
motif1 = mconcat
  [ note (3/4) $ T 0 0 0 0
  , note (1/8) $ T (-1) 0 0 0
  , note (1/8) $ T (-2) 0 0 0
  ]

arpeggiate :: Rational -> Rational -> [T] -> Music
arpeggiate _ _ [] = mempty
arpeggiate _ d2 [t] = note d2 t
arpeggiate d1 d2 (t : ts) = note d1 t <> arpeggiate d1 d2 ts

arp1 :: Music
arp1 = reharmonize (inversion (-3)) $ arpeggiate (1/8) (5/8) [T 0 0 0 0, T 0 1 0 0, T 0 2 0 0, T 0 3 0 0]

arp2 :: Music
arp2 = mconcat
  [ rest (1/8)
  , note (1/8) (T 0 0 0 0)
  , note (1/4) (T 0 (-1) 0 0)
  , note (1/8) (T 1 (-1) 0 0)
  , note (1/4) (T 0 (-1) 0 0)
  , note (1/8) (T (-1) (-1) 0 0)
  ]

backing :: Music
backing = re octave <> re (voice 1 $ note 1 mempty)

waldstein :: Music
waldstein = foldMap (note (1/8))
  [ chordTone 0
  , chordTone 1
  , chordTone 2
  , chordTone 3
  , chordTone 3 <> scaleTone (-1)
  , chordTone 3
  , chordTone 1
  , chordTone 2
  ]

octave :: Music
octave = chord 1 [chordTone 0, chordTone (-3)]

cadences :: Music
cadences = do
  let c = chord 0.25 [chordTone 0, chordTone 1, chordTone 2]
  foldMap ($ c)
    [ id
    , reharmonize (scaleTone 3 <> inversion (-1))
    , reharmonize (scaleTone 4 <> inversion (-2))
    , id
    ]

score :: Music
score = withScale (S.fromList [Af, Bf, C, Df, Ef, F, G]) $ reharmonize (register (-1)) $ mconcat
  [ modulate
      [ scaleTone 3
      , scaleTone 3 <> inversion (-2)
      , scaleTone 3 <> inversion (-1)
      , scaleTone 3 <> inversion (-1)
      , scaleTone 3 <> inversion (-1)
      ] $ cycle [ (re arp1 <> voice 2 motif1), backing <> reharmonize (register 1) (voice 2 waldstein)]
  ]
  -- [ modulate
  --     [ scaleTone 3
  --     , scaleTone 3 <> inversion (-2)
  --     ] $ cycle [ re (voice 2 arp1) <> waldstein ]
  -- ]

--   [ waldstein
--   , reharmonize (scaleTone 3) waldstein
--   , reharmonize (scaleTone 6 <> inversion (-2)) waldstein
--   , reharmonize (scaleTone 9 <> inversion (-3)) waldstein
--   , reharmonize (scaleTone 12 <> inversion (-4)) waldstein
--   , reharmonize (scaleTone 15 <> inversion (-5)) waldstein
--   , reharmonize (scaleTone 18 <> inversion (-6)) waldstein
--   , reharmonize (scaleTone 21 <> inversion (-7)) waldstein
--   ]
  -- withScale (S.fromList [D, E, Fs, G, A, B, Cs]) $
  --   withRoot (Reg 4 D) $
  --     withChord (S.fromList [(-7), (-5), (-2), 0, 7]) $
  --       mconcat
  --         [ modulate [T (-1) 0, T 0 1] $
  --             re (voice 4 motif1) <> arp1
  --         , re (voice 4 $ note 1 mempty) <> offset (-1/8) arp2
  --         ]

main :: IO ()
main = do
  toPdf score
  play $ score
