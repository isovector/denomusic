{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Pieces.SeventyTwo where

import Data.Foldable
import Music
import qualified Data.Set as S

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
    , reharmonize (scaleTone 0)
    , reharmonize (scaleTone 1)
    , reharmonize (scaleTone 2)
    , id
    ]

score :: Music
score = withScale (S.fromList [Af, Bf, C, Df, Ef, F, G]) $
  reharmonize (register (-1)) $ do
    let mods = [ scaleTone 3
               , scaleTone 3 <> inversion (-2)
               , scaleTone 3 <> inversion (-1)
               , scaleTone 3 <> inversion (-1)
               , scaleTone 3 <> inversion (-1)
               ]
    mconcat
      [ modulate mods $ cycle
          [ (re arp1 <> reharmonize (register 1 ) (voice 2 motif1))
          , backing <> reharmonize (register 0) (voice 2 waldstein)
          ]
      , reharmonize (fold mods) $ do
          let ch =
                fork (voice 0 $ chord 1
                  [ chordTone (-3), chordTone 1, chordTone 2
                  ]) $ voice 2 $ chord 1
                  [ chordTone (-3), chordTone 0, chordTone 2
                  ]
          mconcat
            [ stretch (5/8) ch
            , stretch (1/8) $ reharmonize (semiTone (-1)) ch
            , stretch (2/8) $ reharmonize (semiTone (-1)) ch
            ]
      ]

main :: IO ()
main = do
  toPdf score
  play $ score
