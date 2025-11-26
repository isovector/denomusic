{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module APitch2 where

import Data.Bool
import Control.Applicative
import Text.PrettyPrint.HughesPJClass hiding (Mode, (<>), first)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Music.Lilypond.Pitch
import Control.Monad
import APitch hiding (APitch(..))
import APitch qualified as AP
import Data.Kind

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

type APitch :: Type -> Type
data APitch a where
  Absolute :: a -> APitch a
  Color :: Int -> APitch a -> APitch a
  IntervalOn :: Interval -> APitch a -> APitch a
  Degree :: APitch a -> Scale -> Int -> APitch a
  Alta :: Int -> APitch a -> APitch a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)


instance Applicative APitch where
  pure = Absolute
  (<*>) = ap

instance Monad APitch where
  Absolute a >>= f = f a
  Color i p >>= f = Color i (p >>= f)
  IntervalOn i p >>= f = IntervalOn i (p >>= f)
  Degree p s i >>= f = Degree (p >>= f) s i
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
    Degree a s i -> maybeParens (p >= 3) $ mconcat
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
  DegreeF p s i -> toPitch s p $ AP.AP 0 i 0
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

nonempty :: [a] -> [a] -> [a]
nonempty [] as = as
nonempty as _ = as

enumFromToCycle :: (Ord a, Enum a, Bounded a) => a -> a -> [a]
enumFromToCycle from to =
  case compare from to of
    LT -> [from .. to]
    EQ -> [from]
    GT -> [from .. maxBound] <> [minBound .. to]

interval :: PitchName -> Interval -> (PitchName, Accidental, Octaves)
interval pn i =
  let dia = intervalDiaSize i
      pn' = toEnum @PitchName $ mod (fromEnum pn + dia) (fromEnum (maxBound @PitchName) + 1)
      pitches = enumFromToCycle pn pn'
      pitchsemis = sum $ init  $ fmap nextPitchSize pitches
   in ( pn'
      , intervalSize i - pitchsemis
      , bool 0 1 (pn' < pn && dia > 0)
      + bool 0 (-1) (pn' > pn && dia < 0)
      )

cool :: Pitch
cool = eval $ Flat $ Flat $ Degree (pure (Pitch (C, 0, 4)))(mixolydian major) 6
