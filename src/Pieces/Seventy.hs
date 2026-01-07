{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Pieces.Seventy where

import DenoMusic
import GHC.Generics

data Voices
  = Melody
  | Bass
  | Harmony Harmonic
  deriving stock (Eq, Ord, Show, Generic)

instance HasPurpose Voices where
  purpose Melody = MelodicVoice
  purpose Bass = RootVoice
  purpose Harmony {} = HarmonicVoice

instance Bounded Voices where
  minBound = Melody
  maxBound = Harmony maxBound

instance Enum Voices where
  toEnum 0 = Melody
  toEnum 1 = Bass
  toEnum n = Harmony $ toEnum (n - 2)
  fromEnum Melody = 0
  fromEnum Bass = 1
  fromEnum (Harmony h) = 2 + fromEnum h

data Harmonic = H1 | H2 | H3
  deriving stock (Eq, Ord, Show, Enum, Bounded)

harmony :: Music () (T '[3, 7, 12])
harmony =
  line
    [ [2, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [2, 1, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [2, -1, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    ]

motif1 :: Monoid (T m) => Music () (T m)
motif1 = rest (1 / 4) ## (note 1 mempty <* waltz)

maskedHarmony = stamp (delay (1 / 8) $ repeatFor 2 $ alternating quarter) harmony

maskedHarmony2 = stamp (repeatFor 2 $ rest 0.25 ## note (3 / 8) ()) harmony

motificBass :: Music () (T '[3, 7, 12])
motificBass =
  stamp (repeatFor 1 $ alternating quarter) $
    line $
      concat $
        replicate
          4
          [ [0, -7, 0] <$ eighth
          , [0, -10, 0] <$ eighth
          ]

music :: Music Voices (T '[3, 7, 12])
music =
  (<>)
    <$> line
      [ -- Sec A
        chordChange 1 mempty
      , chordChange 1 [-1, 3, 0]
      , chordChange 1 [-1, 4, 0]
      , chordChange 1 mempty
      , -- Sec B
        chordChange 1 (extend $ countervail triad [3]) <> keyChange 1 (invert $ aeolian diatonic)
      , -- , chordChange 1 [3, -7, 0]
        chordChange 1 [2, -4, 0]
      , chordChange 1 [1, -3, 0]
      , chordChange 1 [2, -7, 0]
      , -- Sec C
        chordChange 2 (pow (extend vl3in7) 2)
      , mconcat
          [ chordChange 2 (pow (extend vl3in7) 2)
          , chordChange 2 [0, -1, 0]
          ]
      , mconcat
          [ chordChange 2 (pow (extend vl3in7) 2)
          , chordChange 2 [0, -1, -1]
          -- , modeChange 2 (invert $ aeolian diatonic)
          ]
          -- , modeChange 1 (invert $ aeolian diatonic) <> modeChange 1 (lydian diatonic)
      ]
    <*> line
      [ -- Sec A
        fromVoices $ \case
          Harmony H1 -> maskedHarmony ## maskedHarmony
          Bass -> repeatFor 4 $ note 1 [0, -7, 0]
          Melody ->
            line
              [ fmap (mappend [0, 9, 0]) motif1
              , varyEnd (1 / 4) (mappend [0, -1, 0]) $ fmap (mappend [0, 10, 0]) motif1
              , rest 1
              , varyEnd (1 / 2) (mappend [0, -1, 0]) $ fmap (<> [0, 8, 0]) motif1
              ]
          _ -> mempty
      , -- Sec B
        fromVoices $ \case
          Harmony H1 -> delay (1 / 8) $ double ([1, 0, 0]) $ maskedHarmony ## maskedHarmony
          Harmony H3 -> fmap ((<> [2, 0, 0]) . invert) $ maskedHarmony ## maskedHarmony
          Bass -> repeatFor 4 $ note 1 [0, -7, 0]
          _ -> mempty
      , -- Sec C
        fromVoices $ \case
          Harmony H1 ->
            liftA2
              (<>)
              ( repeatFor 6 $
                  line
                    [ note (3 / 8) [-1, 0, 0]
                    , note (3 / 8) [0, 0, 0]
                    ]
              )
              $ repeatFor 6 maskedHarmony2
          Harmony H2 ->
            repeatFor 6 $
              line
                [ rest (1 / 8)
                , [-3, 0, 0] <$ eighth
                , [-2, 0, 0] <$ eighth
                , rest (5 / 8)
                ]
          Melody ->
            line
              [ [0, 9, 0] <$ quarter
              , [0, 8, 0] <$ quarter
              , rest 0.25
              , [0, 7, 0] <$ quarter
              , rest 0.5
              , [0, 9, 0] <$ quarter
              , [0, 8, 0] <$ quarter
              , [0, 10, 0] <$ quarter
              , [0, 9, 0] <$ quarter
              , rest 0.25
              , [0, 10, 0] <$ quarter
              , [0, 9, 0] <$ quarter
              , [0, 6, 0] <$ quarter
              , rest 0.5
              , [0, 9, 0] <$ quarter
              , [0, 8, 0] <$ quarter
              , rest 0.25
              , [0, 7, 0] <$ quarter
              , rest 0.5
              ]
          Bass -> repeatFor 6 $ note 1 [0, -21, 0]
          _ -> mempty
      ]

double :: Semigroup a => a -> Music () a -> Music () a
double a = fmap (<> a)

main :: IO ()
main =
  defaultMain (Reg 4 C) $
    fst $
      separate (duration music - 0) $
        fmap (mappend $ sink $ aeolian diatonic) $
          music
