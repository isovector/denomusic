{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module Pieces.Musical where

import Data.Traversable
import Control.Monad.State
import Data.Function.Step.Discrete.Open
import Data.Map qualified as M
import Data.Map (Map)
import GHC.Generics
import Pieces.Test hiding (main)
import Data.Set qualified as S
import DenoMusic
import DenoMusic.Types
import DenoMusic.Utils
import DenoMusic.Harmony

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


data Harmonic = Harmony1 | Harmony2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

keys :: Music () (T '[12])
keys = line
  [ note 4 [0]
  , note 4 [0]
  , note 4 [0]
  ]

chords :: Music () (T '[3, 7, 12])
chords = line $ take 12 $ cycle
  [ note 1 [0, 0, 0]
  , note 1 [-1, 3, 0]
  , note 1 [1, -3, 0]
  , note 1 [0, 0, 0]
  ]

inner :: Music () (T '[2, 3])
inner = stretch 2 $ line $ fmap (note 0.25) $ pat [1, -2] $ take 32 $ cycle
  [ 0
  , 1
  , 1
  , -1
  , 0
  , -1
  , -1
  , 1
  ]
-- stretch (7/6) $ line
--   [ line $ fmap (note (1/4)) $ fmap (<> [2, 0]) $ take 8 $ iterate (<> [1, -2]) mempty
--   , line $ fmap (note (1/4)) $ fmap (<> [-2, 0]) $ take 8 $ iterate (<> [-1, 2]) mempty
--   , line $ fmap (note (1/4)) $ fmap (<> [2, 0]) $ take 8 $ iterate (<> [1, -2]) mempty
--   , line $ fmap (note (1/4)) $ fmap (<> [-2, 0]) $ take 8 $ iterate (<> [-1, 2]) mempty
--   ]

pat :: Group a => a -> [Int] -> [a]
pat a = flip evalState mempty . traverse \i -> do
  x <- get
  let x' = x <> pow a i
  put x'
  pure x'

innerVoices :: Music Harmonic (T '[3, 7, 12])
innerVoices = stretch 2 $ fmap (kill (UnsafeMetaScale $ S.fromList [0, 1]) . extend) $ liftA2 (<>) (everyone inner) $ fromVoices \case
  Harmony1 -> line $ fmap (note 0.25) $ pat [1, 0] [1, 0, 0, -1, 0, -1, 1, 1, 0, -1, 1, -1,  0,  -1, -1, -1, 1, -1, 0, 0, 1]
  Harmony2 -> line $ fmap (note 0.25) $ pat [1, 0] [0, 0, 1, 1,  0, -1, 0, 0, 1, 1, 0, -1, -1, 1,   0, 0, 1, -1, 1, -1, 0]

changes :: (Enum v, Ord v, Bounded v) => Music v (T '[3, 7, 12])
changes = (\k c -> sink (sink k) <> c) <$> everyone keys <*> everyone chords

chordify :: (Enum v, Ord v, Bounded v) => Music v (T '[3, 7, 12]) -> Music v (T '[3, 7, 12])
chordify m = (\k c t -> sink (sink k) <> c <> t) <$> everyone keys <*> everyone chords <*> m

-- voices :: Music v v
-- voices = Music 0 pure

unchordy :: T (n ': ns) -> T (n ': ns)
unchordy (_ :> ns) = 0 :> ns

fixBass = withVoice $ \case
  Bass -> fmap unchordy
  Melody -> fmap unchordy
  _ -> id

promote :: (Ord a, Ord b) => (a -> b) -> Music a x -> Music b x
promote = mapVoices

music :: Music Voices (Set (Reg PitchClass))
music =
  fmap (\t -> S.singleton $ elim (MSCons triad $ MSCons diatonic spelledSharp) t (Reg 4 C)) $ fixBass $ chordify $
    mconcat
      [ fromVoices $ \case
          Bass -> delay (-1/8) $ line $ replicate 12 $ line
                  [ note (1/8) [0, -8, 0]
                  , note (7/8) [0, -7, 0]
                  ]
          Melody -> fmap (<> [1, 0, 0]) $ mconcat
                  -- [ contour [0, 1, 0] ((4 *) . crash) $ line $ replicate (3 * 4 * 4) $ note (1/4) ()
                  [ contour [0, 1, 0] ((5 *) . wave) $ line $ take (3 * 4 * 8) $ cycle $ [ rest (1/8), note (1/8) () ]
                  -- , line $ replicate 8 $ contour [1, 0, 0] ((1 *) . valley) $ line $ replicate (3 * 4) $ note (1/4) ()
                  ]
          -- chord space is moving the melody!!
          -- VT -> fmap (<> [-3, 0, 0]) $ contour [1, 0, 0] ((2 *) . wave) $ line $ replicate (3 * 4 * 2) $ note (1 / 2) ()
          -- VA -> fmap (<> [0, 0, 0]) $ contour [1, 0, 0] ((2 *) . valley) $ line $ replicate (3 * 4 * 4) $ note (1 / 4) ()
          _ -> mempty
      , promote Harmony innerVoices
      ]

main :: IO ()
main = do
  toPdf music
  play music





