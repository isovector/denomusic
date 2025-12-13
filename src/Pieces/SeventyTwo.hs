{-# LANGUAGE BlockArguments     #-}
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
sc = S.fromList [A, B, Cs, D, E, Fs, G]

melVoice :: Int
melVoice = 4

globalChanges :: Rational -> Chord (Reg PitchClass)
globalChanges =
  let
    tri = S.fromList [Reg 2 B, Reg 3 Fs, Reg 3 B, Reg 4 D, Reg 5 D]
   in
    thingy
      [ T (-1) 0
      , T (-1) 0
      , T (-1) 0
      , T (-7) 3
      , T 1 0
      , T 1 0
      ] <&> \t -> move sc t tri

localRWS :: (s -> r -> r') -> RWS r' w s a -> RWS r w s a
localRWS f = withRWS $ \z s -> (f s z, s)

voice :: Int -> Music gbl r w a -> Music gbl (Chord r) w a
voice v  = locally $ const $ (!! v) . toList

locally :: (gbl -> r -> r') -> Music gbl r' w a -> Music gbl r w a
locally f (Music m) = Music $ localRWS (\t (gbl, lcl) -> (gbl, f (gbl t) . lcl)) m

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

wait :: Rational -> Music gbl lbl a ()
wait = tell . delay

motif1 :: Voice (Reg PitchClass) (Reg PitchClass) ()
motif1 = do
  emit (3/4) mempty
  emit (1/8) $ T (-1) 0
  emit (1/8) $ T (-2) 0

arpeggiate :: Rational -> Rational -> [Int] -> Voice (Reg PitchClass) (Reg PitchClass) ()
arpeggiate _ _ [] = pure ()
arpeggiate _ tf [i] = emit tf $ T 0 i
arpeggiate t tf (i : is) = emit t (T 0 i) >> arpeggiate t tf is

-- arpeggiate t = traverse_ $ emit t . T 0
-- arpeggiate t = traverse_ $ emit t . T 0

arpegiated :: Voice (Reg PitchClass) (Reg PitchClass) ()
arpegiated = do
  emit 1 mempty
  wait (-1)
  arpeggiate (1/8) 0.5 [0..3]

doubleArp :: Voice (Reg PitchClass) (Reg PitchClass) ()
doubleArp = do
  emit 1 mempty
  wait (-1)

  arpeggiate (1/8) (1/8) [0..3]
  arpeggiate (1/8) (1/8) [3,2..0]

bar :: Voice (Reg PitchClass) (Reg PitchClass) ()
    -> Voice (Reg PitchClass) (Reg PitchClass) ()
    -> Music (Chord (Reg PitchClass)) (Chord (Reg PitchClass)) (Reg PitchClass) ()
bar m1 m2 = do
  hold $ voice 0 m1
  voice melVoice m2


octaveArp :: Voice (Reg PitchClass) (Reg PitchClass) ()
octaveArp = do
  arpeggiate (1/8) (1/4) [0, 1, 2]
  locally (\c -> move1 sc c (T (-1) 0)) $ arpeggiate (1/8) (1/4) [0, 1, 2]



score :: Score (Reg PitchClass)
score = simul
  [ mr_score $ runMusic globalChanges $ do
      replicateM 3 $ bar arpegiated motif1
      bar doubleArp (emit 1 $ T 0 0)
      replicateM 3 $ bar arpegiated motif1
      bar octaveArp (emit 1 $ T 0 0)
      bar octaveArp (wait 1)
      bar motif1 $ arpegiated
  ]

main :: IO ()
main = do
  let s = fmap fromReg score
  toPdf s
  playScore $ scale 1 s
