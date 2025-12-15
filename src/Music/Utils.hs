module Music.Utils where

import Data.Foldable
import Music.Primitives
import Music.Harmony
import Music.Types

dot :: Music -> Music
dot = stretch 1.5

n1 :: Music
n1 = note (1/1) mempty

n2 :: Music
n2 = note (1/2) mempty

n2' :: Music
n2' = dot $ note (1/2) mempty

n4 :: Music
n4 = note (1/4) mempty

n4' :: Music
n4' = dot $ note (1/4) mempty

n8 :: Music
n8 = note (1/8) mempty

n8' :: Music
n8' = dot $ note (1/8) mempty

n16 :: Music
n16 = note (1/16) mempty

n16' :: Music
n16' = dot $ note (1/16) mempty

n32 :: Music
n32 = note (1/32) mempty

n32' :: Music
n32' = dot $ note (1/32) mempty

r1 :: Music
r1 = rest (1/1)

r2 :: Music
r2 = rest (1/2)

r2' :: Music
r2' = dot $ rest (1/2)

r4 :: Music
r4 = rest (1/4)

r4' :: Music
r4' = dot $ rest (1/4)

r8 :: Music
r8 = rest (1/8)

r8' :: Music
r8' = dot $ rest (1/8)

r16 :: Music
r16 = rest (1/16)

r16' :: Music
r16' = dot $ rest (1/16)

r32 :: Music
r32 = rest (1/32)

r32' :: Music
r32' = dot $ rest (1/32)

tuplet :: Foldable t => t Music -> Music
tuplet ts =
  let l = toList ts
      len = sum $ fmap duration l
   in stretch (1 / len) $ fold l

ct :: Int -> T
ct = chordTone

semi :: Int -> T
semi = semiTone

st :: Int -> T
st = scaleTone

reg :: Int -> T
reg = register

up :: Int -> (Int -> T) -> Music -> Music
up i f = reharmonize $ f i

down :: Int -> (Int -> T) -> Music -> Music
down i f = reharmonize $ f $ negate i

(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 9 #
