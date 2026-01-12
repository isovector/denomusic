{-# LANGUAGE OverloadedLists #-}

module Pieces.Structure where

import DenoMusic.Harmony
import DenoMusic
import Pieces.Lefty (withChords, liner)
import Control.Monad.RWS

type M = T '[3, 7, 12]

data SATB = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data ComposerState = ComposerState
  { cs_idx :: Int
  , cs_t :: M
  }

harmony :: Music () M
harmony = line
  [ line $ fmap (note 1 . extend . pow vl3in7) [0,1,2,3,-3,-1,5,7]
  , fmap (<> sink (aeolian diatonic)) $ line
      $ fmap (note 1 . extend . pow vl3in7) [0,1,2,3,-3,-1,5,7]
  ]

spread :: Music SATB M -> Music SATB M
spread = withVoice $ \case
  VS -> fmap (<> [0, 7, 0])
  VA -> fmap (<> [-1, 0, 0])
  VT -> fmap (<> [-1, 0, 0])
  VB -> fmap (<> [0, -7, 0])

phrase1MelodyM :: Music () M
phrase1MelodyM = fmap (<> [-1, 0, 0]) $ line
  [ [0, 0, 0] <$ eighth
  , [1, 0, 0] <$ eighth
  , [2, 0, 0] <$ eighth
  , [2, -1, 0] <$ eighth
  , [2, 0, 0] <$ eighth
  , [1, 0, 0] <$ eighth
  , [0, 0, 0] <$ eighth
  , [1, 0, 0] <$ eighth
  ]

phrase1Mel :: Music () M
phrase1Mel =
  line
    [ phrase1MelodyM
    , fmap invert phrase1MelodyM
    , phrase1MelodyM
    , mempty <$ quarter
    , mempty <$ quarter
    , [0, -1, 0] <$ quarter
    , [0, -2, 0] <$ quarter
    , fmap (<> [1, 0, 0]) $ phrase1MelodyM
    , fmap (<> [1, 0, 0]) $ fmap invert phrase1MelodyM
    , [1, -7, 0] <$ quarter
    , [2, -8, 0] <$ quarter
    , [2, -7, 0] <$ quarter
    , [2, -7, 1] <$ quarter
    , [-1, 0, 0] <$ whole
    ]

phrase2MelodyM :: Music () M
phrase2MelodyM = fmap (<> [-1, 0, 0]) $ line
  [ rest (3/8)
  , [2, 0, 0] <$ eighth
  , [2, -1, 0] <$ eighth
  , [0, 1, 0] <$ eighth
  , [1, 0, 0] <$ quarter
  ]

crash :: Music SATB M
crash = repeatFor 4 $ everyone $ note 4 mempty

phrase2Mel :: Music () M
phrase2Mel = repeatFor 8 $ phrase2MelodyM ## fmap invert phrase2MelodyM

vl2in3 :: T '[2, 3]
vl2in3 = toT @2 @3 $ VoiceLeading (-1)


harmonics :: Music () (T '[2, 3])
harmonics = line $ fmap (note 0.25) $ fmap (pow vl2in3) $ fmap (+ 1)
  [ 0
  , -1
  , 1
  , 2
  , 0
  , 1
  , -1
  , 1
  ]

cutFor :: Rational -> Music v a -> Music v a
cutFor d = delay d . trimStart d

music :: Music SATB M
music = liftA2 (<>) (everyone harmony) $ spread $ fromVoices $ \case
    VS -> phrase1Mel ## phrase2Mel
    VA -> delay 4 $ repeatFor 4 $ fmap (kill (UnsafeMetaScale [0, 1]) . extend) $  fmap (<> [1, 0]) harmonics
    VT -> delay 4 $ repeatFor 4 $ fmap (kill (UnsafeMetaScale [0, 1]) . extend) harmonics
    VB -> line $ replicate 100 $ line
      [ note 1 mempty
      , note (5/8) mempty
      , rest (2/8)
      , note (1/8) mempty
      ]
    -- VT -> delay 2 $ line $ replicate 100 $ line
    --   [ mempty <$ quarter
    --   , [-1, 0, 0] <$ quarter
    --   , mempty <$ quarter
    --   , [-1, 0, 0] <$ quarter
    --   ]
    _ -> line $ replicate 100 $ note 1 mempty

main :: IO ()
main = withChords (Reg 4 C) harmony music
