{-# LANGUAGE DerivingStrategies #-}

module Westergaard where

import Control.Lens hiding (Empty)
import Control.Applicative
import Control.Monad
import Euterpea (Pitch, PitchClass (..))
import Legacy hiding (main)
import Rhythm


drum :: Rhythm Pitch
drum = im
  [ (C, 2)
  , (C, 2)
  , (C, 2)
  , (C, 2)
  ]


denoted :: [a] -> Rhythm b -> Rhythm a
denoted as r = do
  z <- set (partsOf traversed) (fmap Right as) $ fmap Left r
  case z of
    Left _ -> empty
    Right x -> pure x

tune :: [Pitch]
tune =
  [ (C, 4)
  , (C, 4)
  , (D, 4)
  , (C, 4)
  , (F, 4)
  , (E, 4)
  , (C, 4)
  , (C, 4)
  , (D, 4)
  , (C, 4)
  , (G, 4)
  , (F, 4)
  , (C, 4)
  , (C, 4)
  , (C, 5)
  , (A, 4)
  , (F, 4)
  , (E, 4)
  , (D, 4)
  , (Bf, 4)
  , (Bf, 4)
  , (A, 4)
  , (F, 4)
  , (G, 4)
  , (F, 4)
  ]

newtype Music a = Music [Rhythm a]
  deriving stock (Functor)
  deriving newtype (Show, Semigroup, Monoid)

bars :: Int -> Rhythm a -> Music a
bars n rh = Music $ do
  let width = recip $ fromIntegral n
  i <- [0 .. n - 1]
  let lo = fromIntegral i * width
      hi = lo + width
  pure $ getSpan (Interval (Closed lo) (Open hi)) rh



getMusic :: Music a -> Rhythm a
getMusic (Music as) = tuplet as

playMusic :: Music Pitch -> IO ()
playMusic (Music as) = playBars (length as) $ tuplet as

main :: IO ()
main = playMusic $ bars 1 drum

dup :: Rhythm a -> Rhythm a
dup = (>>= \x -> im [x, x])


strongBeats :: Rhythm a -> Rhythm a
strongBeats = overlay (const id) $ im $ replicate 4 ()


chorded :: Rhythm [Pitch] -> Rhythm Pitch
chorded = (chord =<<)

brokenChord :: Rhythm [Pitch] -> Rhythm [Pitch]
brokenChord r = do
  ps <- r
  im [take 2 ps, drop 2 ps]

data HasLH = NoLH | QuarterLH | EightLH | StrikeLH

song :: Rhythm Pitch
song = tuplet $ do
  has_lh <-
    mconcat
      [ replicate 2 NoLH
      , replicate 4 QuarterLH
      , join $ replicate 4 [StrikeLH, EightLH]
      ]
  let rh = chorded $ brokenChord $ im $ replicate 6 $ minor C 5
  pure $
    Par rh $
      chorded $ case has_lh of
        NoLH -> Empty
        QuarterLH -> im $ replicate 4 $ minor C 4
        StrikeLH ->
          tuplet
            [ pure $ minor C 2
            , brokenChord $ pure $ minor C 4
            , brokenChord $ pure $ minor C 4
            , brokenChord $ pure $ minor C 4
            ]
        EightLH -> brokenChord $ im $ replicate 4 $ minor C 4
