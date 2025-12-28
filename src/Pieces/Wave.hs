{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE OverloadedLists        #-}
{-# OPTIONS -fno-warn-type-defaults #-}

module Pieces.Wave where

import Data.Semigroup
import Data.Set qualified as S
import Data.Set (Set)
import DenoMusic hiding (line)
import DenoMusic qualified as D

line :: (Enum v, Bounded v) => [M v] -> M v
line = D.line


data SATB = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type Std = T '[3, 7, 12]
type M a = Music a Std


motifAc1 :: M ()
motifAc1 = line
  [ note (1/8) [0, 1, 0]
  , note (3/8) [1, 0, 0]
  ]

motifAc2 :: M ()
motifAc2 = line
  [ note (1/8) [0, 0, 0]
  , note 0.25 [0, 1, 0]
  , rest (1/8)
  ]

motifA :: M ()
motifA = line
  [ motifAc1
  , motifAc2
  ]

motifB :: M ()
motifB = line
  [ note (3/8) [1, 0, 0]
  , note (1/8) [1, (-1), 0]
  , note 0.25 [1, 0, 0]
  , note 0.25 [2, 0, 0]
  ]

s1p1 :: M SATB
s1p1 = fromVoices $ \case
  VS -> motifA ## fmap invert motifA
  VB -> line
        [ rest (1/8)
        , note (6/8) mempty
        , note (6/8) [1, 0, 0]
        --
        , note (3/8) [(-1), 0, 0]
        -- , note (1/2) $ [1, 0, 0]
        ]
  _ -> mempty


s1p2 :: M SATB
s1p2 =
  mconcat
    [ s1p1
    , fromVoices $ \case
        VT -> line
          [ rest (1/8)
          , note (3/8) [1, 0, 0]
          , rest (1/8)
          , note (3/8) [1, 0, 0]
          , rest (1/8)
          , note (3/8) [2, 0, 0]
          , rest (1/8)
          , note (3/8) [1, 0, 0]
          ]
        _ -> mempty
    ]


s1p3 :: M SATB
s1p3 =
  mconcat
    [ s1p2
    , fromVoices $ \case
        VA -> line
          [ rest (1/4)
          , motifAc1
          , rest (3/4)
          ]
        _ -> mempty
    ]


s2p1 :: M SATB
s2p1 = fromVoices $ \case
  VS -> motifB ## fmap (mappend [2, 0, 0] . invert) motifB
  VA -> motifAc1 ## rest (1/2) ## motifAc1 ## rest (1/2)
  _ -> mempty


s2p2 :: M SATB
s2p2 = fromVoices $ \case
  VS -> motifB ## fmap (mappend [3, 0, 0] . invert) motifB
  VT -> motifAc1 ## rest (1/2) ## motifAc1 ## motifAc2
  _ -> mempty

repeatd :: (Enum v, Bounded v, Semigroup a) => Int -> Music v a -> Music v a
repeatd n = D.line . replicate n

s2p3 :: M SATB
s2p3 = mconcat
  [ fmap (mappend [1, 0, 0]) s2p1
  , fromVoices $ \case
      VT -> repeatd 2 $ note 0.25 [0, 0, 0] ## note 0.75 [0, 0, 0]
      VB -> repeatd 2 $ note 0.25 [0, 0, 0] ## note 0.75 [0, 0, 0]
      _ -> mempty
  ]


music :: M SATB
music = line
  [ liftA2 (<>) (everyone $ line
      $ take 20 $ cycle
      [ note 1 mempty
      , note 1 (extend $ invert $ stimes 2 vl3in7)
      ]
      ) $ line
        [ s1p1, s1p3, fmap invert s1p3, fmap invert s1p2
        , s2p1, s2p2, s2p3
        ]
  ]

main :: IO ()
main = do
  let score
        = fmap S.singleton
        $ fmap (flip (elim standard) (Reg 4 C))
        $ splitVoices
        $ music
  toPdf score
  play $ stretch 1.2 score


splitVoices :: M SATB -> M SATB
splitVoices m = liftA2 (<>) m $ fromVoices $ pure . extend . \case
  VS -> 2 :> Nil
  VA -> 1 :> Nil
  VT -> (-2) :> Nil
  VB -> (-3) :> Nil
