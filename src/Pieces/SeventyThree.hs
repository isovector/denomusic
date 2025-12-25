module Pieces.SeventyThree where

import Data.Semigroup
import Data.Foldable
import qualified Data.Set as S
import Music

bass1 :: Music
bass1 = voice 0 $ mconcat
  [ n8
  , n4 # up 2 ct
  , r2
  , r8
  -- , n8 # up 2 ct # down 1 st
  -- , n4 # up 1 ct
  , r2
  ] # down 1 reg

v1 :: Music
v1 =  voice 1 $
  mconcat
    [ r4
    , n8
    , n8 # up 1 ct
    , n2 # up 2 ct
    , r4
    , r4
    ] # down 1 ct

v2 :: Music
v2 =  voice 1 $
  mconcat
    [ r4
    , n8
    , n8 # up 2 ct
    , n2 # up 2 ct # down 1 st
    , r4
    , r4
    ] # down 1 ct

moves = mconcat
  [ move (T (-3) 1 0 0)
  , move (T (-2) 2 0 0)
  , move (T (-2) 1 0 0)
  ]

score :: Music
score =
  -- tempo (1/4) 60 $
    withScale (S.fromList [C, D, Ef, F, G, A, B]) $
    withChord (S.fromList [Reg 4 C, Reg 4 Ef, Reg 4 G]) $ do
      let t1 =
                do
                  let vs = [ v1
                          , move (T (-3) 1 0 0) <> v1
                          , move (T (-2) 2 0 0) <> v1
                          , move (T (-2) 1 0 0) <> v2
                          ]
                  fork (foldMap (const bass1) vs) $ fold vs
      mconcat
        [ t1
        , fork (voice 0 $ t1 <> reharmonize (chordTone (-2)) t1) $ mappend moves $ voice 2 $ stimes 2 $ phrase $ mconcat
            [ r4
            , n4
            , n4 # up 2 ct
            , n4 # up 2 ct # up 1 st
            , n1 # up 2 ct
            , n4 # up 1 ct
            , n2 # up 1 ct # down 1 st
            , r2
            , r4
            , move (T (-5) 2 0 0)
            ]

        ]

main :: IO ()
main = do
  toPdf score
  play score
