{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module Pieces.Seventy where

import Data.Map qualified as M
import Data.Function.Step.Discrete.Open (SF(..))
import Data.Map.Monoidal qualified as MM
import Data.Set qualified as S
import GHC.Generics
import DenoMusic
import DenoMusic.Types
import DenoMusic.Harmony
import DenoMusic.Rhythms

data Voices
  = Melody
  | Bass
  | Harmony Harmonic
  deriving stock (Eq, Ord, Show, Generic)

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

chordChange :: Monoid (T ns) => Rational -> T (n ': ns) -> Music Voices (T (n ': ns))
chordChange d t'@(_ :> zs) = fromVoices $ note d . \case
  Melody -> mempty
  Bass -> (0 :> zs)
  Harmony{} -> (t')

modeChange :: Monoid (T ns) => Rational -> T ns -> Music Voices (T (n ': ns))
modeChange d t = everyone $ note d $ sink t

voices :: Finite v => Music v v
voices = fromVoices pure



data Harmonic = H1 | H2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

mixolydian :: T '[7, 12]
mixolydian = [4, (-7)]

aeolian :: T '[7, 12]
aeolian = [5, (-9)]

harmony :: Music () (T '[3, 7, 12])
harmony =
  line
    [ [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [2, 1, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [2, -1, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    ]

toMusic :: Reg PitchClass -> T '[7, 12] -> Music v (T '[3, 7, 12]) -> Music v (Set (Reg PitchClass))
toMusic root (sd :> cd :> Nil) =
  fmap $ \t ->
    S.singleton
      $ elim
          standard
          (t <> [0, sd, cd])
          root

motif1 :: Monoid (T m) => Music () (T m)
motif1 = rest (1/4) ## (note 1 mempty <* waltz)

maskedHarmony = stamp (delay (1/8) $ repeatFor 2 $ alternating quarter) harmony

motificBass :: Music () (T '[3, 7, 12])
motificBass = stamp (repeatFor 1 $ alternating quarter) $ line $ concat $ replicate 4
  [ [0, -7, 0] <$ eighth
  , [0, -10, 0] <$ eighth
  ]

music :: Music Voices (T '[3, 7, 12])
music =
  (<>)
  <$>
  line
    [ -- Sec A
      chordChange 1 mempty
    , chordChange 1 [-1, 3, 0]
    , chordChange 1 [-1, 4, 0]
    , chordChange 1 mempty
      -- Sec B
    , chordChange 1 [3, -7, 0] <> modeChange 1 (invert aeolian)
    -- , chordChange 1 [3, -7, 0]
    , chordChange 1 [2, -4, 0]
    , chordChange 1 [1, -3, 0]
    , chordChange 1 [2, -7, 0]

      -- Sec C
    , chordChange 4 mempty
    , chordChange 4 [-2, 0, 0] <> modeChange 4 [0, 5, -1]
    ]
  <*>
  line
    [ -- Sec A
      fromVoices $ \case
        Harmony H1 -> maskedHarmony ## maskedHarmony
        Bass -> repeatFor 4 $ note 1 [0, -7, 0]
        Melody -> line
          [ fmap (mappend [0, 9, 0]) motif1
          , varyEnd (1/4) (mappend [0, -1, 0]) $ fmap (mappend [0, 10, 0]) motif1
          , rest 1
          , varyEnd (1/2) (mappend [0, -1, 0]) $ fmap (<> [0, 8, 0]) motif1
          ]
        _ -> mempty

      -- Sec B
    , fromVoices $ \case
        Harmony H1 -> maskedHarmony ## maskedHarmony
        Harmony H2 -> delay (1/8) $ double ([1, 0, 0]) $ maskedHarmony ## maskedHarmony
        Bass -> repeatFor 4 $ note 1 [0, -7, 0]
        _ -> mempty

      -- Sec C
    , fromVoices $ \case
        Harmony H1 -> mempty
        Harmony H2 -> maskedHarmony ## maskedHarmony
        Bass -> repeatFor 4 motificBass
        _ -> mempty
    ]

double :: Semigroup a => a -> Music () a -> Music () a
double a = fmap (<> a)


main :: IO ()
main = do
  let s = toMusic (Reg 4 C) aeolian music
  toPdf s
  play s

