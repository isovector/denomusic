{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module Score where

import Data.String (IsString(..))
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.List (sortOn)
import Data.Maybe
import Data.Monoid.Action
import Data.Semigroup
import Data.Tree.DUAL hiding (flatten)
import Data.Tree.DUAL qualified as D
import Euterpea (Music (..), rest)
import Euterpea qualified as E


data Ann = Phrase
  deriving stock Show


data Envelope a = Envelope
  { e_scale :: a
  , e_offset :: a
  , e_voice :: Maybe Voice
  }
  deriving stock (Show, Functor)

instance Num a => Semigroup (Envelope a) where
  Envelope s1 o1 v1 <> Envelope s2 o2 v2 =
    Envelope (s1 * s2) (s1 * o2 + o1) $ v2 <|> v1

instance Num a => Monoid (Envelope a) where
  mempty = Envelope 1 0 Nothing

newtype Score a = Score
  { unScore :: DUALTree (Envelope Rational) (Sum Rational) Ann a
  }
  deriving newtype (Show, Functor)

instance Semigroup (Score a) where
  sa@(Score a) <> Score b =
    Score $ a <> applyD (Envelope 1 (duration sa) Nothing) b

instance Monoid (Score a) where
  mempty = Score mempty


instance Num a => Action (Envelope a) (Sum a) where
  act e (Sum s) = Sum $ e_scale e * s

data Voice
  = Soprano | Alto | Tenor | Bass | LeftHand | RightHand | Custom String
  deriving stock (Eq, Ord, Show)

instance IsString Voice where
  fromString = Custom

voice :: Voice -> Score a -> Score a
voice v = Score . applyD (Envelope 1 0 $ Just v) . unScore

duration :: Score a -> Rational
duration = getSum . fromMaybe mempty . getU . unScore

note :: a -> Score a
note = Score . leaf (Sum 1)

tile :: Rational -> a -> Score a
tile d = scale d . note

delay :: Rational -> Score a
delay d = Score $ leafU $ Sum d

scale :: Rational -> Score a -> Score a
scale d (Score s) = Score $ applyD (Envelope d 0 Nothing) s

scaleTo :: Rational -> Score a -> Score a
scaleTo d t =
  case duration t of
    0 -> mempty
    dur -> scale (d / dur) t

lh :: Score a -> Score a
lh = voice LeftHand

rh :: Score a -> Score a
rh = voice RightHand

inv :: Score a -> Score a
inv t = let d = duration t in delay (- d) <> t <> delay (- d)

re :: Score a -> Score a
re t = t <> delay (- duration t)

co :: Score a -> Score a
co t = delay (- duration t) <> t


fork :: Score a -> Score a -> Score a
fork a b = re a <> b

join :: Score a -> Score a -> Score a
join a b = a <> co b

phrase :: Score a -> Score a
phrase = Score . annot Phrase . unScore


newtype Simul a = Simul { getSimul :: Score a }

instance Semigroup (Simul a) where
  Simul a <> Simul b = Simul $ re a <> re b

instance Monoid (Simul a) where
  mempty = Simul mempty


simul :: [Score a] -> Score a
simul [] = mempty
simul as = foldr1 fork as

flatten :: Score a -> [(a, Envelope Rational)]
flatten = sortOn (e_offset . snd) . D.flatten . unScore


toMusic :: Score a -> Music a
toMusic s = do
  let es = flip fmap (flatten s) $ \(e, Envelope d t _v) -> rest t :+: E.note d e
  foldr (:=:) (rest 0) es


chord :: [a] -> Score a
chord = simul . fmap (tile 1)

im :: [a] -> Score a
im = foldMap (tile 1)


playScore :: (E.ToMusic1 a, NFData a) => Score a -> IO ()
playScore
  = E.playDev 2
  . toMusic

