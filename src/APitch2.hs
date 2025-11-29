{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module APitch2
  ( eval
  , fromSemis
  , APitch(.., Sharp, Flat)
  , Interval(..)
  , Scale(..)
  , ScaleForm(..)
  ) where

import APitch (ScaleForm(..), nextPitchSize)
import Control.Monad
import Data.Bool
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Music.Lilypond.Pitch
import Text.PrettyPrint.HughesPJClass hiding (Mode, (<>), first)


data Scale a = Scale ScaleForm (APitch a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)


data Interval where
  Invert :: Interval -> Interval
  Aug :: Interval -> Interval
  Dim :: Interval -> Interval
  P1 :: Interval
  Min2 :: Interval
  Maj2 :: Interval
  Min3 :: Interval
  Maj3 :: Interval
  P4 :: Interval
  P5 :: Interval
  Min6 :: Interval
  Maj6 :: Interval
  Min7 :: Interval
  Maj7 :: Interval
  deriving stock (Eq, Ord, Show)

fromSemis :: Int -> Interval
fromSemis 0 = P1
fromSemis 1 = Min2
fromSemis 2 = Maj2
fromSemis 3 = Min3
fromSemis 4 = Maj3

instance Pretty Interval where
  pPrint = \case
    P1 -> "P1"
    Min2 -> "m2"
    Maj2 -> "M2"
    Min3 -> "m3"
    Maj3 -> "M3"
    P4 -> "P4"
    P5 -> "P5"
    Min6 -> "m6"
    Maj6 -> "M6"
    Min7 -> "m7"
    Maj7 -> "M7"
    Aug x -> "A" <> pPrint x
    Dim x -> "d" <> pPrint x
    Invert x -> "-" <> pPrint x

data APitch a where
  Absolute :: a -> APitch a
  Color :: Int -> APitch a -> APitch a
  IntervalOn :: Interval -> APitch a -> APitch a
  Degree :: Scale a -> Int -> APitch a
  Alta :: Int -> APitch a -> APitch a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)


instance Applicative APitch where
  pure = Absolute
  (<*>) = ap

instance Monad APitch where
  Absolute a >>= f = f a
  Color i p >>= f = Color i (p >>= f)
  IntervalOn i p >>= f = IntervalOn i (p >>= f)
  Degree (Scale sf p) i >>= f = Degree (Scale sf $ p >>= f) i
  Alta i p >>= f = Alta i (p >>= f)

pattern Flat :: APitch a -> APitch a
pattern Flat a = Color (-1) a

pattern Sharp :: APitch a -> APitch a
pattern Sharp a = Color 1 a

$(makeBaseFunctor [''APitch])

instance Pretty a => Pretty (APitch a) where
  pPrintPrec l p = \case
    Absolute a -> pPrintPrec prettyNormal 10 a
    Color i a -> maybeParens (p >= 9) $ mconcat
      [ text $ case compare i 0 of
          LT -> replicate (negate i) '♭'
          EQ -> "♮"
          GT -> replicate i '♯'
      , pPrintPrec l 8 a
      ]
    IntervalOn (Invert i) a -> maybeParens (p >= 5) $ mconcat
      [ pPrintPrec l 10 a
      , "-"
      , pPrint i
      ]
    IntervalOn i a -> maybeParens (p >= 5) $ mconcat
      [ pPrintPrec l 10 a
      , "+"
      , pPrint i
      ]
    Alta i a -> maybeParens (p >= 9) $ mconcat
      [ pPrintPrec l 8 a
      , text $ case compare i 0 of
          LT -> replicate (negate i) 'ᵇ'
          EQ -> ""
          GT -> replicate i 'ᵃ'
      ]
    Degree (Scale s a) i -> maybeParens (p >= 3) $ mconcat
      [ brackets $ mconcat
          [ pPrint a
          , ":"
          , text (s_name s)
          ]
      , text $ show i
      ]

eval :: APitch Pitch -> Pitch
eval = cata $ \case
  AbsoluteF p -> p
  ColorF i (Pitch (a, b, c)) -> Pitch (a, b + i, c)
  DegreeF (Scale (ScaleForm s _) (eval -> Pitch (pc, a, o))) i ->
    let (pc', da, dx) =
          diatonic pc i $
            case i < 0 of
              True -> 12 - sum (take (abs i) $ cycle $ reverse s)
              False -> sum $ take i $ cycle s
     in Pitch
          ( pc'
          , a + da
          , sum
              [ o
              , dx
              , div i (length s)
              , bool 0 1 (i < 0)
              ]
          )
  AltaF i (Pitch (a, b, c)) -> Pitch (a, b, c + i)
  IntervalOnF i (Pitch (pc, a, o)) ->
    let (pc', da, dx) = interval pc i
     in Pitch (pc', a + da, o + dx)

intervalDiaSize :: Interval -> Int
intervalDiaSize = \case
  P1 -> 0
  Min2 -> 1
  Maj2 -> 1
  Min3 -> 2
  Maj3 -> 2
  P4 -> 3
  P5 -> 4
  Min6 -> 5
  Maj6 -> 5
  Min7 -> 6
  Maj7 -> 6
  Aug x -> intervalDiaSize x
  Dim x -> intervalDiaSize x
  Invert x -> negate $ intervalDiaSize x

intervalSize :: Interval -> Int
intervalSize = \case
  P1 -> 0
  Min2 -> 1
  Maj2 -> 2
  Min3 -> 3
  Maj3 -> 4
  P4 -> 5
  P5 -> 7
  Min6 -> 8
  Maj6 -> 9
  Min7 -> 10
  Maj7 -> 11
  Aug x -> intervalSize x + 1
  Dim x -> intervalSize x - 1
  Invert x -> 12 - intervalSize x

deriving stock instance Bounded PitchName

sigmod :: Integral a => a -> a -> a
sigmod x y = signum x * mod (abs x) y

enumFromToCycle :: (Ord a, Enum a, Bounded a) => a -> a -> [a]
enumFromToCycle from to =
  case compare from to of
    LT -> [from .. to]
    EQ -> [from]
    GT -> [from .. maxBound] <> [minBound .. to]

diatonic :: PitchName -> Int -> Int -> (PitchName, Accidental, Octaves)
diatonic pn dia size =
  let pn' = enumMod dia pn
      pitches = enumFromToCycle pn pn'
      pitchsemis = sum $ init  $ fmap nextPitchSize pitches
   in ( pn'
      , sigmod (size - pitchsemis) 12
      , bool 0 1 (pn' < pn && dia > 0)
      + bool 0 (-1) (pn' > pn && dia < 0)
      )

interval :: PitchName -> Interval -> (PitchName, Accidental, Octaves)
interval pn i = diatonic pn (intervalDiaSize i) (intervalSize i)


data PitchClass = PC | PCs | PD | PDs | PE | PF | PFs | PG | PGs | PA | PAs | PB
  deriving stock (Eq, Ord, Show, Enum, Bounded)



comparePitch :: Pitch -> Pitch -> Ordering
comparePitch (Pitch (pc1, a1, r1)) (Pitch ((pc2, a2, r2))) =
  mconcat
    [ compare r1 r2
    , compare (normalize pc1 a1) (normalize pc2 a2)
    ]

enumMod :: forall a. (Enum a, Bounded a) => Int -> a -> a
enumMod a pn = toEnum $ mod (fromEnum pn + a) (fromEnum (maxBound @a) + 1)

normalize :: PitchName -> Int -> PitchClass
normalize pn a = enumMod a $ toPc pn

toPc :: PitchName -> PitchClass
toPc C = PC
toPc D = PD
toPc E = PE
toPc F = PF
toPc G = PG
toPc A = PA
toPc B = PB
