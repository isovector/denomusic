module Pieces.Seventy where

import APitch hiding (APitch)
import APitch2
import Data.Music.Lilypond.Pitch
import Score

type P = APitch Pitch

p :: Int -> PitchName -> Int -> APitch Pitch
p r pc a = pure $ Pitch (pc, a, r)


dotted :: Score a -> Score a
dotted = scale (1.5)

undotted :: Score a -> Score a
undotted = scale (2/3)

motif1 :: Scale Pitch -> Score P
motif1 s = mconcat
  [ tile 0.125 $ Degree s 3
  , delay 0.25
  , tile 0.125 $ Degree s 2
  , delay 0.125
  , dotted $ tile 0.25 $ Degree s 0
  , delay 0.125
  ]

motif1phrase1 :: Score P
motif1phrase1 = mconcat
  [ motif1 $ Scale major $ p 4 C 0
  , undotted $ motif1 $ Scale major $ p 3 B (-1)
  , fmap invertDegree $ scale 0.5 $ motif1 $ Scale hungarianMinor $ p 4 C 0
  ]

motif1phrase2 :: Score P
motif1phrase2 = mconcat
  [ motif1 $ Scale major $ p 4 C 0
  , undotted $ motif1 $ Scale major $ p 3 B (-1)
  , tile 0.25 $ p 3 E (-1)
  , dotted $ tile 0.25 $ p 3 F 0
  ]

invertDegree :: APitch a -> APitch a
invertDegree (Degree s x) = Degree s $ negate x
invertDegree a = a

arpchord :: [a] -> Score a
arpchord as = mconcat
  [ flip foldMap (zip [0..] as) $ \(i, a) -> do
      let offset = 0.025 * fromInteger i
      re $ mconcat
        [ delay offset
        , tile (1 - offset) a
        ]
  , delay 1
  ]


phrase2 :: Scale Pitch -> Score P
phrase2 s = scale 0.5 $ mconcat
  [ arpchord
      [ Degree s 0
      , Degree s 1
      , Degree s 4
      ]
  , chord
      [ Degree s 0
      , Degree s 2
      , Degree s 4
      ]

  , delay 0.5

  , arpchord
      [ Degree s (-1)
      , Degree s 0
      , Degree s 3
      ]
  , chord
      [ Degree s (-2)
      , Degree s 0
      , Degree s 3
      ]

  , delay 0.5

  , arpchord
      [ Degree s (-2)
      , Degree s (-1)
      , Degree s 2
      ]
  , delay 0.5
  , tile 0.5 $ p 5 C 0
  , tile 0.5 $ p 4 B 0
  , tile 0.5 $ p 4 G 0
  , arpchord
      [ p 3 B 0
      , p 4 E 0
      ]
  , delay (-1)
  , chord
      [ p 2 A 0
      , p 3 A 0
      ]
  ]

score :: Score P
score = mconcat
  [ motif1phrase2
  , phrase2 $ Scale major $ p 5 C 0
  , motif1phrase1
  ]


main :: IO ()
main = playScore $ fmap eval score

