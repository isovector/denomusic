{-# LANGUAGE OverloadedLists #-}

module Pieces.Test where

import Data.Set qualified as S
import Data.Set (Set)
import DenoMusic
import DenoMusic.Utils

wave :: Rational -> Rational
wave = toRational . sin . (* (2 * pi)) . fromRational

valley :: Rational -> Rational
valley = toRational . cos . (* (2 * pi)) . fromRational

crash :: Rational -> Rational
crash = valley . (/ 2)

crest :: Rational -> Rational
crest = wave . (/ 2)

sigmoid :: Rational -> Rational
sigmoid = toRational . (\x -> 1 / (1 + exp (-x))) . fromRational . subtract 3 . (* 6)

main :: IO ()
main = do
  let m = stretch 2 $ fmap (\t -> S.singleton $ elim standard t (Reg 4 C)) $ contour [1, 0, 0] ((* 2) . wave) $ line $ take 6 $ cycle $
            [ note (1/16) ()
            -- , note (1/8) ()
            ]
  toPdf $ m <> pure (S.singleton $ Reg 2 C)
  play $ m <> pure (S.singleton $ Reg 2 C)
