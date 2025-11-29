{-# LANGUAGE DerivingStrategies     #-}
{-# OPTIONS_GHC -fno-warn-x-partial #-}

-- | As per https://www.madmusicalscience.com/
module MadMusic where

import Notation
import Data.Monoid
import Data.List (inits)
import Score
import Debug.Trace
import Data.Foldable
import Euterpea (PitchClass(..))
import qualified Data.Set as S
import Data.Set (Set)


data T = T
  { extrinsic :: Int
  , intrinsic :: Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup T where
  T e1 i1 <> T e2 i2 = T (e1 + e2) (i1 + i2)

instance Monoid T where
  mempty = T 0 0

type Scale = Set
type Chord = Set

data Reg a = Reg
  { getReg :: Int
  , unReg :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

fromReg :: Reg a -> (a, Int)
fromReg (Reg i a) = (a, i)

extrPred :: Ord a => Scale a -> Reg a -> Reg a
extrPred sc (Reg r a)
  | a == S.findMin sc = Reg (r - 1) $ S.findMax sc
  | otherwise = Reg r $ S.findMax $ snd $ S.partition (>= a) sc

extrSucc :: Ord a => Scale a -> Reg a -> Reg a
extrSucc sc (Reg r a)
  | a == S.findMax sc = Reg (r + 1) $ S.findMin sc
  | otherwise = Reg r $ S.findMin $ snd $ S.partition (<= a) sc


extrMove :: Ord a => Scale a -> Int -> Reg a -> Reg a
extrMove sc n r
  = case compare n 0 of
      LT -> head $ drop (abs n) $ iterate (extrPred sc) r
      EQ -> r
      GT -> head $ drop (abs n) $ iterate (extrSucc sc) r

invert :: Ord a => Int -> Chord (Reg a) -> Chord (Reg a)
invert i c = S.map (extrMove (S.map unReg c) i) c


move :: (Show a, Ord a) => Scale a -> T -> Chord (Reg a) -> Chord (Reg a)
move sc (T e i) c =
  let c' = S.map (extrMove sc e) c
      sc' = S.map unReg c'
   in S.map (extrMove sc' i) c'

sc = S.fromList [A, B,C, Cs, Ds,E, Fs,G, Gs]
tri = S.fromList [Reg 3 Cs, Reg 4 E, Reg 4 A]


voices :: Chord a -> [a]
voices c =
  case S.minView c of
    Nothing -> []
    Just (a, c') -> a : voices c'

main :: IO ()
main = do
  let moves = take 7 $ cycle
        [ T (6) (-2)
        , T (2) (-1)
        , T (-8) (2)
        ]

  let score =
        fmap fromReg $ scale 0.5 $ flip foldMap (inits moves) $ \ms -> do
          let c = appEndo (flip foldMap ms $ Endo . move sc) $ tri
              sc' = S.map unReg c
              (mel : ch@(mid : _)) = reverse $ voices c
          fork (chord $ toList $ invert 0 $ S.fromList ch) $
            fork
              ( mconcat
                  [ delay 0.25
                  , tile 0.25 $ extrMove sc' 1 mid
                  , delay 0.25
                  , tile 0.25 $ extrMove sc' (-1) mid
                  ]
              ) $
              mconcat
              [ tile 0.25 $ extrMove sc' 0 mel
              , tile 0.25 $ extrMove sc (5) mel
              , tile 0.125 $ extrMove sc (4) mel
              , tile 0.125 $ extrMove sc (5) mel
              , tile 0.25 $ extrMove sc' 1 mel
              ]
  toPdf score
  playScore score
