module Pieces.Guaraldi where

import Data.List
import Data.Semigroup
import qualified Data.Set as S
import Music
import Music.Utils


setup :: Music -> Music
setup
  = withScale (S.fromList [Af, A, Bf, B, C, Df, D, Ef, E, F, Gf, G])
  . withChord (S.fromList [Reg 3 F, Reg 4 Af, Reg 4 C])

lick :: Music
lick = mconcat
  [ mconcat
    [ n8 # down 1 st # up 1 ct
    , n8 # up 1 ct
    , stretch (1/3) $ mconcat
        [ r4
        , n4 # up 2 ct
        , n4 # up 3 ct
        ]
    , n16 # up 3 ct # up 2 st
    , n16 # up 3 ct
    , n16 # up 4 ct
    , n16 # up 5 ct # up 1 st
    ]
  , mconcat
    [ n16 # down 2 st
    , n16 # down 3 ct
    , n16 # down 3 st
    , n16 # down 2 ct
    , n16 # down 3 ct
    , n8' # down 4 ct # up 1 st
    , r8
    , stretch (1/3) $ mconcat
        [ n8 # down 5 ct # up 1 st
        , n8 # down 4 ct # up 1 st
        , n8 # down 3 ct # up 2 st
        ]
    ] # reharmonize (T 8 (-1) 2 0)
  ]

score :: Music
score = setup $ mconcat
  [ negative lick # up 2 reg
  ]

main :: IO ()
main = do
  toPdf score
  play score
