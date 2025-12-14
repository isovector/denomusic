{-# LANGUAGE DerivingStrategies                   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-x-partial               #-}

-- | As per https://www.madmusicalscience.com/
module MadMusic where

import Data.Semigroup
import qualified Data.Set as S
import Data.Set (Set)
import Euterpea qualified as E

data PitchClass
  = Cff | Cf | C | Cs | Css
  | Dff | Df | D | Ds | Dss
  | Eff | Ef | E | Es | Ess
  | Fff | Ff | F | Fs | Fss
  | Gff | Gf | G | Gs | Gss
  | Aff | Af | A | As | Ass
  | Bff | Bf | B | Bs | Bss
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

toStupidEuterpeaPitchClass :: PitchClass -> E.PitchClass
toStupidEuterpeaPitchClass Aff = E.Aff
toStupidEuterpeaPitchClass Af = E.Af
toStupidEuterpeaPitchClass A = E.A
toStupidEuterpeaPitchClass As = E.As
toStupidEuterpeaPitchClass Ass = E.Ass
toStupidEuterpeaPitchClass Bff = E.Bff
toStupidEuterpeaPitchClass Bf = E.Bf
toStupidEuterpeaPitchClass B = E.B
toStupidEuterpeaPitchClass Bs = E.Bs
toStupidEuterpeaPitchClass Bss = E.Bss
toStupidEuterpeaPitchClass Cff = E.Cff
toStupidEuterpeaPitchClass Cf = E.Cf
toStupidEuterpeaPitchClass C = E.C
toStupidEuterpeaPitchClass Cs = E.Cs
toStupidEuterpeaPitchClass Css = E.Css
toStupidEuterpeaPitchClass Dff = E.Dff
toStupidEuterpeaPitchClass Df = E.Df
toStupidEuterpeaPitchClass D = E.D
toStupidEuterpeaPitchClass Ds = E.Ds
toStupidEuterpeaPitchClass Dss = E.Dss
toStupidEuterpeaPitchClass Eff = E.Eff
toStupidEuterpeaPitchClass Ef = E.Ef
toStupidEuterpeaPitchClass E = E.E
toStupidEuterpeaPitchClass Es = E.Es
toStupidEuterpeaPitchClass Ess = E.Ess
toStupidEuterpeaPitchClass Fff = E.Fff
toStupidEuterpeaPitchClass Ff = E.Ff
toStupidEuterpeaPitchClass F = E.F
toStupidEuterpeaPitchClass Fs = E.Fs
toStupidEuterpeaPitchClass Fss = E.Fss
toStupidEuterpeaPitchClass Gff = E.Gff
toStupidEuterpeaPitchClass Gf = E.Gf
toStupidEuterpeaPitchClass G = E.G
toStupidEuterpeaPitchClass Gs = E.Gs
toStupidEuterpeaPitchClass Gss = E.Gss

data T = T
  { t_extrinsic :: Int
  , t_intrinsic :: Int
  , t_register :: Int
  , t_semitone :: Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup T where
  T e1 i1 r1 s1 <> T e2 i2 r2 s2 = T (e1 + e2) (i1 + i2) (r1 + r2) (s1 + s2)

instance Monoid T where
  mempty = T 0 0 0 0

type Scale = Set
type Chord = Set

data Reg a = Reg
  { getReg :: Int
  , unReg :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

fromReg :: Reg a -> (a, Int)
fromReg (Reg i a) = (a, i)

withReg :: (Int -> Int) -> Reg a -> Reg a
withReg f (Reg r a) = Reg (f r) a

extrPred :: Ord a => Scale a -> Reg a -> Reg a
extrPred sc (Reg r a)
  | a == S.findMin sc = Reg (r - 1) $ S.findMax sc
  | otherwise = Reg r $ S.findMax $ snd $ S.partition (>= a) sc

extrSucc :: Ord a => Scale a -> Reg a -> Reg a
extrSucc sc (Reg r a)
  | a == S.findMax sc = Reg (r + 1) $ S.findMin sc
  | otherwise = Reg r $ S.findMin $ snd $ S.partition (<= a) sc

nTimes :: Int -> (a -> a) -> (a -> a) -> a -> a
nTimes n f g = appEndo $
  case compare n 0 of
    LT -> stimes (abs n) $ Endo f
    EQ -> mempty
    GT -> stimes (abs n) $ Endo g


extrMove :: Ord a => Scale a -> Int -> Reg a -> Reg a
extrMove sc n r
  = case compare n 0 of
      LT -> head $ drop (abs n) $ iterate (extrPred sc) r
      EQ -> r
      GT -> head $ drop (abs n) $ iterate (extrSucc sc) r

invert :: Ord a => Int -> Chord (Reg a) -> Chord (Reg a)
invert i c = S.map (extrMove (S.map unReg c) i) c

move1 :: (Enum a, Ord a) => Scale a -> Chord (Reg a) -> T -> Reg a -> Reg a
move1 sc ch (T e i r s) a
  = fmap (nTimes s pred succ)
  $ withReg (+ r)
  $ extrMove (S.map unReg $ S.map (extrMove sc e) ch) i
  $ extrMove sc e a

move :: (Enum a, Ord a) => Scale a -> T -> Chord (Reg a) -> Chord (Reg a)
move sc (T e i r s) c =
  let c' = S.map (extrMove sc e) c
      sc' = S.map unReg c'
   in S.map (fmap (nTimes s pred succ) . withReg (+ r)) $ S.map (extrMove sc' i) c'


