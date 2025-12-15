{-# LANGUAGE BlockArguments     #-}

module Pieces.Seventy where

import Data.List
import Data.Semigroup
import qualified Data.Set as S
import Music
import Music.Utils


setup :: Music -> Music
setup
  = withScale (S.fromList [A, B, C, D, E, F, G])
  . withChord (S.fromList [Reg 2 F, Reg 3 A, Reg 4 C, Reg 4 A])

echo :: Music -> Music
echo m = accent $ mconcat
  [ m
  , rest $ duration m * 0.5
  , stretch 0.5 m
  ]

p1 :: Music
p1 = phrase $ mconcat
  [ n8
  , n8 # up 2 ct
  , n8 # up 3 ct
  , n8 # up 3 ct # down 1 st
  , n8 # up 3 ct
  , n8 # up 1 ct
  , n8 # up 2 ct
  , n8 # up 2 ct # down 1 st
  ]

triad :: Music
triad =
  simul
    [ n1
    , n1 # up 1 ct
    , n1 # up 2 ct
    ]

score :: Music
score = setup $ mconcat
  [ voice 1 p1
  , move (T (-2) (1) 0 0)
  , voice 1 p1
  , move (T (-2) (1) 0 0)
  , voice 1 p1
  ]

v1 :: Music -> Music
v1 = voice 1

v3 :: Music -> Music
v3 = voice 3 . echo


main :: IO ()
main = do
  toPdf score
  play score
