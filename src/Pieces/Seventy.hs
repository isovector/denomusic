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


data Harmonic = H1 | H2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

drive :: Music () ()
drive = line $ concat $ replicate 4
  [ eighth
  , rest (1 / 8)
  ]

mixolydian :: T '[7, 12]
mixolydian = [4, (-7)]

aeolian :: T '[7, 12]
aeolian = [5, (-9)]

repeatd :: (Enum v, Bounded v, Ord v, Semigroup a) => Int -> Music v a -> Music v a
repeatd n = line . replicate n

harmony :: Music Harmonic (T '[3, 7, 12])
harmony = fromVoices $ \case
  H1 -> line
    [ [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [2, 1, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [2, -1, 0] <$ eighth
    , [2, 0, 0] <$ eighth , [0, 0, 0] <$ eighth
    ]
  H2 -> mempty

data P = P
  { ph :: Music Harmonic (T '[3, 7, 12])
  , pm :: Music () (T '[7, 12])
  , pb :: Music () (T '[7, 12])
  }
  deriving stock Generic
  deriving (Semigroup, Monoid) via Generically P

type M = [P]

toMusic :: Reg PitchClass -> T '[7, 12] -> M -> Music Voices (Set (Reg PitchClass))
toMusic root t ps = line $ fmap (unP t root) ps

unP :: T '[7, 12] -> Reg PitchClass -> P -> Music Voices (Set (Reg PitchClass))
unP t0 root (P ph pm pb) = fmap (\t -> S.singleton $ elim (MSCons diatonic spelledFlat) (t <> t0) root) $ mconcat
  [ voice Melody pm
  , voice Bass pb
  , fmap (kill triad) $ mapVoices Harmony ph
  ]

motif1 :: Music () (T '[7, 12])
motif1 = rest (1/4) ## (note 1 [0, 0] <* waltz)

step :: [(Rational, Maybe a)] -> Maybe a -> Music () a
step ts a
  = Music (maximum $ 0 : fmap fst ts)
  $ MM.singleton ()
  $ Voice
  $ SF (M.fromList $ (0, Nothing) : ts) a

type Finite a = (Enum a, Bounded a, Ord a)

varyLast :: Finite v => Rational -> (a -> a) -> Music v a -> Music v a
varyLast t f m =
  everyone (step [(duration m - t, Just id), (duration m, Just f)] Nothing) <*> m

silencing :: Finite v => Music () () -> Music v a -> Music v a
silencing f m = everyone f *> m

maskOff :: Music () () -> Music () ()
maskOff m = line $ mconcat $ replicate 4 $
  [ m
  , rest $ duration m
  ]

maskedHarmony = silencing (delay (1/8) $ repeatd 1 $ maskOff quarter) harmony

music :: Music Voices (Set (Reg PitchClass))
music = toMusic (Reg 4 C) aeolian $
  -- [ mempty
  --     { ph = harmony
  --     , pb = note 1 [-7, 0] ## note 1 [-7, 0]
  --     , pm = rest 2
  --     }
  [ mempty
      { ph = fmap (<> extend vl3in7) maskedHarmony
      , pb = note 1 [-4, 0] ## note 1 [-4, 0]
      , pm = fmap (<> [10, 0]) motif1
              ## (fmap (<> [10, 0]) $ varyLast (1/4) (<> [-1, 0]) motif1)
      -- , pm = line
      --     [ rest 1
      --     , [6, 0] <$ quarter
      --     , [6, 0] <$ quarter
      --     , [6, 0] <$ quarter
      --     , [6, 0] <$ eighth
      --     , [7, 0] <$ eighth
      --     ]
      }
  , mempty
      { ph = fmap (<> extend (invert vl3in7)) maskedHarmony
      , pb = note 2 [-3, 0]
      , pm = fmap (<> [9, 0]) motif1
              ## (fmap (<> [9, 0]) $ varyLast (1/4) (<> [-1, 0]) motif1)
      }
  , mempty
      { ph = silencing (delay (3/8) $ maskOff quarter) harmony
      , pb = mapVoices (const ())
           $ fmap (kill triad . (<> [-4, 0, 0]))
           $ silencing (repeatd 1 $ maskOff quarter) harmony
      , pm = line
          [ note 0.5 [7, 0]
          , note 0.25 [8, 0]
          , note 0.25 [9, 0]
          , note 0.5 [10, 0]
          ]
      }
  , mempty
      { ph = maskedHarmony
      , pb = note 2 [-7, 0]
      , pm = line
          [ note 0.5 [7, 0]
          , note 0.25 [8, 0]
          , note 0.25 [9, 0]
          , note 0.5 [10, 0]
          ]
      }
  , mempty
      { ph = mempty
      , pb = note 2 [-7, 0]
      , pm = line
          [ note 0.5 [7, 0]
          , note 0.25 [8, 0]
          , note 0.25 [9, 0]
          , note 0.5 [10, 0]
          , note 0.5 [11, 0]
          ]
      }
  ]
-- fromVoices $ \case
--   Bass -> line
--     [ drive *> pure (S.singleton $ Reg 4 C)
--     , drive *> pure (S.singleton $ Reg 4 C)
--     ]
--   Melody -> line
--     [ rest (1/8)
--     , drive *> pure (S.singleton $ Reg 4 E)
--     , drive *> pure (S.singleton $ Reg 4 E)
--     ]
--   _ -> mempty


main :: IO ()
main = do
  toPdf music
  play music

