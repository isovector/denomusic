{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE OverloadedLists        #-}
{-# OPTIONS -fno-warn-type-defaults #-}

module Pieces.Two where

import Data.List (intersperse)
import Data.Semigroup
import Data.Set qualified as S
import DenoMusic

-- | Repeat a motif, each time transformed by the given monoid element. The list gives nestings of the
selfSimilar :: Monoid a => a -> Int -> Music v a -> Music v a
selfSimilar a n m = line $ zipWith (\z -> liftA2 (<>) z . pure) (replicate n m) $ iterate (<> a) mempty

data SATB = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)


music :: Music SATB (Set (Reg PitchClass))
music = fmap S.singleton $ fmap (flip (elim standard) (Reg 4 C)) abstract

type Western = T '[3, 7, 12]

splitVoices :: Music SATB Western -> Music SATB Western
splitVoices m = liftA2 (<>) m $ fromVoices $ pure . extend . \case
  VS -> 2 :> Nil
  VA -> 1 :> Nil
  VT -> 0 :> Nil
  VB -> (-3) :> Nil

motif :: Music () Western
motif = line
  [ note 0.25 mempty
  , note (3/8) [1, 0, 0]
  , note 0.125 [0, -1, 0]
  , note 0.25 [0, 0, 0]
  ]

phrasing :: Music () Western -> Music () Western
phrasing m
  = selfSimilar (stimes 2 $ extend $ invert vl3in7) 2 $ line
      [ selfSimilar [1, 0, 0] 1 m
      , stretch (1/2) $ selfSimilar [1, 0, 0] 3 $ fmap (<> [1, 0, 0]) m
      ]

phrase1 :: Music () Western
phrase1 = phrasing motif

phrase2 :: Music () Western
phrase2 = line
  [ note 0.25 mempty
  , note 0.25 mempty
  , rest 0.5
  ]

interjection :: Music SATB Western
interjection = everyone $ line
  [ note 0.25 mempty
  , note 0.25 mempty
  , note 0.25 mempty
  ]

keyChanges :: Music SATB Western
keyChanges = everyone $ line
  [ note 2 mempty
  , note 4 $ sink vl7in12
  , note 2 $ stimes 3 $ sink vl7in12
  , note 2 $ stimes 4 $ sink vl7in12
  , note 2 $ stimes 5 $ sink vl7in12
  , note 1 $ stimes 6 $ sink vl7in12
  , note 1 $ stimes 7 $ sink vl7in12
  , note 0.5 $ stimes 8 $ sink vl7in12
  , note 0.5 $ stimes 9 $ sink vl7in12
  , note 2 $ stimes 8 $ sink vl7in12
  ]

abstract :: Music SATB Western
abstract
  = splitVoices
  $ liftA2 (<>) keyChanges
  $ line
  $ intersperse interjection
  $ split 3
  $ fromVoices $ \case
    VB -> line $ replicate 3 $ phrasing $ line
      [ note (1/4) mempty
      , note (1/4) [1, 0, 0]
      , note (1/4) [2, 0, 0]
      , note (1/4) [0, 0, 0]
      ]
    VT -> line
        [ phrase1
        , phrasing phrase2
        , phrasing phrase2
        ]
    VA -> line
        [ rest (duration phrase1)
        , phrase1
        , phrasing phrase2
        ]
    VS -> line
        [ rest (2 * duration phrase1)
        , phrase1
        ]

main :: IO ()
main = do
  toPdf music
  play music
