{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module Pieces.Three where

import Data.Map.Monoidal qualified as MM
import Data.Function.Step.Discrete.Open
import DenoMusic
import DenoMusic.Types
import DenoMusic.Utils (neighbor, rootMotion)
import Pieces.Seventy qualified as Two


data V = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance HasPurpose V where
  purpose = \case
    VS -> MelodicVoice
    VB -> RootVoice
    _ -> HarmonicVoice

type W = T '[3, 7, 12]

persist :: Rational -> a -> Music () a
persist t = Music t . MM.singleton () . Voice . step 0 Nothing . Just

harmonicField :: Music V W
harmonicField = line
  [ chordChange 1 mempty
  , mconcat
      [ chordChange 1 [-1, 3, 0]
      , rootMotion 1 [0, -3, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 5, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  , mconcat
      [ chordChange 1 [-2, 3, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  , mconcat
      [ chordChange 1 [0, 0, 0]
      , rootMotion 1 [0, -4, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 3, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 0, 0]
      , rootMotion 1 [0, -5, 0]
      ]
  , mconcat
      [ chordChange 1 [0, 2, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 3, 0]
      ]
  , mconcat
      [ chordChange 1 [0, 3, 0]
      ]
  , mconcat
      [ chordChange 1 [0, 2, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 4, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  , mconcat
      [ chordChange 1 [-2, 4, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  , mconcat
      [ chordChange 1 [-3, 4, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 0, 0]
      , rootMotion 1 [0, 0, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 5, 0]
      ]
  , mconcat
      [ chordChange 1 [-1, 3, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  , mconcat
      [ chordChange 1 [-2, 6, 0]
      , rootMotion 1 [0, -7, 0]
      ]
  ]

r :: Music v a -> Music () b
r = rest . duration

motif :: Music () W
motif = line
  [ [0, 0, 0] <$ dotted quarter
  , r eighth
  , [0, 1, 0] <$ dotted quarter
  , r eighth
  , [0, 0, 0] <$ quarter
  , [0, -1, 0] <$ dotted quarter
  , r eighth
  , [0, 0, 0] <$ eighth
  , [0, -1, 0] <$ eighth
  , [0, -2, 0] <$ quarter
  ]



music :: Music V W
music =
  liftA2 (<>) harmonicField $ fromVoices $ \case
    VB -> line
      [ [0, 0, 0] <$ dotted half
      , [-2, 0, 0] <$ quarter
      , [-1, 0, 0] <$ whole
      , mempty <$ half
      , [0, -1, 0] <$ quarter
      , [0, -2, 0] <$ quarter
      -- 4
      , [2, 0, 0] <$ dotted half
      , [0, -1, 0] <$ quarter
      , mempty <$ dotted half
      , [1, 0, 0] <$ quarter
      -- 6
      , mempty <$ whole
      , rest 1
      -- 8
      , [0, -7, 0] <$ whole
      , [-1, -7, 0] <$ whole
      , [-1, -7, 0] <$ whole
      , [0, -7, 0] <$ half
      , [0, -7, 0] <$ dotted quarter
      , [0, -8, 0] <$ eighth
      ]
    VS -> line
      [ motif
      , r $ dotted half
      , mempty <$ dotted quarter
      , r eighth
      , [0, 1, 0] <$ dotted quarter
      , r eighth
      , [0, 0, 0] <$ quarter
      , [0, -1, 0] <$ dotted quarter
      , r eighth
      , [0, 0, 0] <$ eighth
      , [0, 1, 0] <$ eighth
      , [0, 2, 0] <$ quarter
      , rest 0.25
      , neighbor 1 (<> [0, -1, 0]) ([0, 0, 0] <$ quarter)
      , rest 1
      -- 8
      , fmap invert motif
      , r quarter
      , [0, -2, 0] <$ quarter
      , [0, -2, 0] <$ quarter
      , [0, -2, -1] <$ dotted quarter
      , [0, -2, 0] <$ eighth
      , [0, -2, 0] <$ quarter
      ]
    VT -> line
      [ rest 0.25
      , mempty <$ quarter
      , [1, 0, 0] <$ half
      , rest 0.25
      , [1, 0, 0] <$ quarter
      , [0, 0, 0] <$ quarter
      , [0, -1, 0] <$ quarter
      , rest 1
      , [3, -1, 0] <$ whole
      -- 5
      , rest 0.25
      , rest 0.25
      , rest 0.25
      -- , mempty <$ quarter
      -- , [-2, 0, 0] <$ quarter
      , rest 0.25
      , rest 0.25
      , neighbor 1 (<> [0, -1, 0]) (mempty <$ quarter)
      , rest 0.25
      , neighbor 1 (<> [0, -1, 0]) ([0, 0, 0] <$ quarter)
      , neighbor 1 (<> [0, -1, 0]) ([-1, 0, 0] <$ quarter)
      -- 8
      , [-2, 0, 0] <$ half
      , [-3, 0, 0] <$ half
      , [-2, 0, 0] <$ half
      , [-3, 0, 0] <$ half
      , rest 0.5
      , [-1, 0, 0] <$ quarter
      , [-1, -1, 0] <$ quarter
      , [-1, -2, 0] <$ quarter
      , [-1, -3, 0] <$ quarter
      , [-1, -4, 0] <$ half
      ]
    VA -> line
      [ rest 2
      , rest 0.25
      , [-1, 0, 0] <$ quarter
      , [0, 0, 0] <$ quarter
      , [1, 0, 0] <$ quarter
      , [3, -1, 0] <$ whole
      -- 5
      , rest 0.25
      , neighbor 1 (<> [0, -1, 0]) (mempty <$ quarter)
      -- , mempty <$ half
      , rest 0.25
      , r half
      , r half -- $ neighbor 1 (<> [0, -1, 0]) ([0, 0, 0] <$ quarter)
      , neighbor 1 (<> [0, -1, 0]) ([3, 0, 0] <$ quarter)
      , r half
      -- 8
      , mempty <$ half
      , [1, 0, 0] <$ half
      , mempty <$ half
      , [1, 0, 0] <$ half
      , neighbor 1 (<> [0, -1, 0]) ([-1, 0, 0] <$ quarter)
      , rest 0.5
      ]
    _ -> mempty


longer :: (Finite v, Semigroup a) => Music v a -> Music v a -> Music v a
longer m1 m2 =
  let d = max (duration m1) (duration m2)
   in liftA2 (<>) (repeatFor d m1) (repeatFor d m2)

spreadVoices :: Music V W -> Music V W
spreadVoices = liftA2 (<>) $ fromVoices $ pure . \case
  VS -> [0, 9, 0]
  VA -> [0, 0, 0]
  VT -> [-1, 0, 0]
  VB -> [0, -7, 0]

main :: IO ()
main = defaultMainSharp (Reg 4 D) $ fmap (mappend $ sink $ aeolian diatonic) $ (fst $ separate (duration music) $ spreadVoices music) ## mapVoices (\case
  Two.Melody -> VS
  Two.Bass -> VB
  Two.Harmony Two.H1 -> VT
  Two.Harmony Two.H2 -> VA
  Two.Harmony Two.H3 -> VA

  ) Two.music
