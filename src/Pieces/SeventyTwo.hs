{-# LANGUAGE OverloadedLists #-}

module Pieces.SeventyTwo where

import DenoMusic
import DenoMusic.Utils (neighbor, rootMotion)

type M = (T '[3, 7, 12])

harmony :: Music () M
harmony =
  const
    <$> line
        [ [1, 0, 0] <$ dotted quarter
        , [0, 0, 0] <$ eighth
        , [1, 0, 0] <$ quarter
        , [-1, 0, 0] <$ quarter
        ]
    <*> tango

data V = Mel | Root | Bass | Chords
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance HasPurpose V where
  purpose Mel = MelodicVoice
  purpose Root = RootVoice
  purpose Bass = HarmonicVoice
  purpose Chords = HarmonicVoice

maskedHarmony :: Music () M
maskedHarmony =
  stamp (repeatFor 1 $ delay 0.25 $ alternating quarter) harmony

changes :: Music V M
changes = line
  [ chordChange 1 mempty
  , mconcat
      [ chordChange 1 [1, -2, 0]
      , keyChange 1 (invert $ aeolian diatonic)
      ]
  , mconcat
      [ chordChange 1 [1, -4, 0]
      ]
  , mconcat
      [ chordChange 1 [2, -3, 0]
      , keyChange 1 (invert $ aeolian diatonic)
      ]
  , mconcat
      [ chordChange 1 mempty
      ]
  , mconcat
      [ chordChange 1 $ extend $ pow vl3in7 (-1)
      , keyChange 1 (invert $ aeolian diatonic)
      ]
  ]

longer :: (Finite v, Semigroup a) => Music v a -> Music v a -> Music v a
longer m1 m2 =
  let d = max (duration m1) (duration m2)
   in liftA2 (<>) (repeatFor d m1) (repeatFor d m2)

phrase1 :: Music () M
phrase1 = line
  [ neighbor 1 (<> [0, -1, 0]) $ [0, 10, 0] <$ quarter
  , rest 0.25
  , neighbor 1 (<> [0, -1, 0]) $ [0, 9, 0] <$ quarter
  , [0, 9, 0] <$ quarter
  , [0, 8, 0] <$ quarter
  , [0, 7, 0] <$ quarter
  , neighbor 1 (<> [0, -1, 0]) $ [0, 7, 0] <$ quarter
  , rest 0.25
  , [0, 6, 0] <$ quarter
  ]

trimEnd :: Rational -> Music v a -> Music v a
trimEnd t m = fst $ separate (duration m - t) m

music :: Music V M
music = fmap (<> (sink $ aeolian diatonic)) $
  longer changes $ line
  [ fromVoices $ \case
      Mel -> mempty
      -- phrase1 ## line
      --   [ rest 0.25
      --   , [0, 9, 0] <$ quarter
      --   , [0, 8, 0] <$ quarter
      --   , [0, 7, 0] <$ quarter
      --   , rest 0.5
      --   , [0, 9, 0] <$ quarter
      --   , [0, 8, 0] <$ quarter
      --   , [0, 7, 0] <$ quarter
      --   , [0, 8, 0] <$ quarter
      --   , [0, 9, 0] <$ eighth
      --   , [0, 10, 0] <$ half
      --   ]
      Bass -> repeatFor 6 $
        -- fmap (<> [-3, 0, 0]) harmony
        fmap (<> [-3, 0, 0]) $ line
          [ [1, 0, 0] <$ dotted quarter
          , [2, 0, 0] <$ eighth
          , [1, 0, 0] <$ quarter
          , [1, 0, 0] <$ quarter
          , harmony
          ]
      Root -> repeatFor 6 $ line
        [ [0, -7, 0] <$ dotted quarter
        , rest $ duration $ eighth ## quarter ## quarter
        ]
      Chords -> repeatFor 6 $
        line
                [ [0, 0, 0] <$ eighth
                , [0, 0, 0] <$ quarter
                , [2, 0, 0] <$ eighth
                , rest 0.5
                , [2, 0, 0] <$ eighth
                , [2, 0, 0] <$ quarter
                , [1, 0, 0] <$ eighth
                , [1, -1, 0] <$ quarter
                , rest 0.25
                ]
  ]

main :: IO ()
main = defaultMain (Reg 4 C) music
