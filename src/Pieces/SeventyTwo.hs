{-# LANGUAGE DerivingStrategies #-}

module Pieces.SeventyTwo where

import Data.Functor.Identity
import Debug.Trace
import Data.Semigroup
import Data.Foldable
import Control.Monad
import Notation
import qualified Data.Set as S
import Data.Functor
import Score hiding (Voice)
import MadMusic
import Euterpea (PitchClass(..))
import Control.Monad.RWS

newtype Music gbl lcl w a = Music
  { unMusic :: RWS (Rational -> gbl, gbl -> lcl) (Score w) Rational a
  } deriving newtype (Functor, Applicative, Monad)

type Voice lcl = Music (Chord lcl) lcl


data MusicResult r w a = MusicResult
  { mr_result :: a
  , mr_score :: Score w
  , mr_final :: r
  }

runMusic :: (Rational -> gbl) -> Music gbl gbl w a -> MusicResult gbl w a
runMusic f (Music m) =
  let (a, t, w) = runRWS m (f, id) 0
   in MusicResult a w (f t)

instance MonadWriter (Score w) (Music gbl lcl w) where
  tell s = Music $ do
    tell s
    modify' (+ duration s)
  listen (Music ma) = Music $ listen ma
  pass (Music ma) = Music $ pass ma

seekG :: Rational -> Music gbl lcl w gbl
seekG t = Music $ ($) <$> fmap fst ask <*> fmap (+ t) get

seek :: Rational -> Music gbl lcl w lcl
seek t' = Music $ do
  (gbl, lcl) <- ask
  t <- get
  pure $ lcl $ gbl $ t + t'

peekG :: Music gbl lcl w gbl
peekG = seekG 0

peek :: Music gbl lcl w lcl
peek = seek 0

sc :: Scale PitchClass
sc = S.fromList [A, As, B, C, Cs, D, Ds, E, F, Fs, G, Gs]

globalChanges :: Rational -> Chord (Reg PitchClass)
globalChanges =
  let
    tri = S.fromList [Reg 3 Cs, Reg 4 E, Reg 4 A]
   in
    thingy
      [ T (4) (-2)
      , T (2) (-1)
      , T (-6) (2)
      ] <&> \t -> move sc t tri

globalChanges2 :: Chord (Reg PitchClass) -> Rational -> Chord (Reg PitchClass)
globalChanges2 tri =
  thingy
    [ T (-3) (2)
    , T (1) (-1)
    , T (2) (-1)
    ] <&> \t -> move sc t tri

localRWS :: (r -> r') -> RWS r' w s a -> RWS r w s a
localRWS f = withRWS $ \z s -> (f z, s)

voice :: Int -> Music gbl r w a -> Music gbl (Chord r) w a
voice v (Music m) = Music $ localRWS (\(gbl, lcl) -> (gbl, (!! v) . toList . lcl)) m

hold :: Music gbl lcl w a -> Music gbl lcl w a
hold m = Music $ do
  t <- get
  a <- unMusic m
  t' <- get
  put t
  tell $ delay (t - t')
  pure a

withReg :: (Int -> Int) -> Reg a -> Reg a
withReg f (Reg r a) = Reg (f r) a

leading :: Rational -> Rational -> T -> Voice (Reg PitchClass) (Reg PitchClass) ()
leading offset r t = do
  ch <- seekG offset
  c <- seek offset
  tell $ tile r $ move1 sc ch t c

emit :: Rational -> T -> Voice (Reg PitchClass) (Reg PitchClass) ()
emit = leading 0


bass2Bars :: Voice (Reg PitchClass) (Reg PitchClass) ()
bass2Bars = do
  emit 0.25 $ T 0 0
  emit 0.25 $ T 0 1
  emit 0.25 $ T 0 2
  leading 0.25 0.25 $ T (-1) 3
  emit 0.25 $ T 0 3
  emit 0.25 $ T 0 2
  emit 0.25 $ T 0 1
  leading 0.25 0.25 $ T (1) 0

musicly = do
  replicateM 2 $ voice 0 bass2Bars

score :: Score (Reg PitchClass)
score =
  let m1 = mr_score $ runMusic globalChanges musicly
      m2 = mr_score $ runMusic globalChanges $ replicateM 2 $ do
             tell $ delay 0.5
             voice 1 $ bass2Bars
      m3 = mr_score $ runMusic globalChanges $ do
             tell $ delay 1
             voice 2 $ bass2Bars
   in re m1 <> re m2 <> re m3

main :: IO ()
main = do
  let s = fmap fromReg score
  toPdf s
  playScore s
