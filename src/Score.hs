{-# LANGUAGE DerivingStrategies          #-}
{-# LANGUAGE DerivingVia                 #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Score where
  -- ( Score
  -- , playScore

  -- -- * Construction
  -- , bar
  -- , asBars

  -- -- * Combinators
  -- , strut
  -- , delay
  -- , par
  -- , parL
  -- ) where

import Test.QuickCheck hiding (scale)
import Test.QuickCheck qualified as Q
import Debug.Trace
import Data.Function
import Data.Ratio
import Data.Maybe
import Control.Monad
import GHC.Generics
import Data.Semigroup
import Euterpea.IO.MIDI.Play (playDev)
import Euterpea (Music (..), Pitch, rest)
import Euterpea qualified as E
import Data.Tree.DUAL
import Data.Void
import Data.Monoid.Action
import Linear.V3
import Linear.Matrix
import Rhythm
import Test.Hspec hiding (after)
import Test.Hspec.QuickCheck

data Envelope a = Envelope
  { e_before :: Maybe (Max a)
  , e_after :: Maybe (Max a)
  }
  deriving stock (Eq, Generic, Functor, Show)
  deriving (Semigroup, Monoid) via Generically (Envelope a)

newtype Matrix a = Matrix { getMatrix :: M33 a }
  deriving newtype (Eq, Show)

instance Num a => Semigroup (Matrix a) where
  Matrix x <> Matrix y = Matrix $ x !*! y

instance Num a => Monoid (Matrix a) where
  mempty = Matrix identity



newtype Score a = Score
  { unScore :: DUALTree (Matrix Rational) (Envelope Rational) Void a
  }
  deriving stock (Eq, Functor, Show)
  deriving newtype (Semigroup, Monoid)

instance Fractional a => Action (Matrix a) (Envelope a) where
  act (Matrix m) (Envelope (Just (Max x)) (Just (Max y))) =
    -- let V3 start width _ = m !* V3 (-x) (y + x) 1
    --  in Envelope (Just $ Max (-start)) (Just $ Max (width + start))
    let V3 _ x' _ = m !* V3 0 x 1
        V3 _ y' _ = m !* V3 0 y 1
     in Envelope (Just $ Max $ x') (Just $ Max $ y')
  act _ _ = undefined

getEnv :: Score a -> Maybe (Envelope Rational)
getEnv = getU . unScore

getEnvB :: Score a -> Maybe Rational
getEnvB = (fmap getMax . e_before) <=< getU . unScore

getEnvA :: Score a -> Maybe Rational
getEnvA = (fmap getMax . e_after) <=< getU . unScore

delay :: Rational -> Score a -> Score a
delay r = Score . applyD (Matrix $ V3 (V3 1 0 r)
                                      (V3 0 1 0)
                                      (V3 0 0 1)) . unScore

scale :: Rational -> Score a -> Score a
scale r = Score . applyD (Matrix $ V3 (V3 r 0 0)
                                      (V3 0 r 0)
                                      (V3 0 0 1)) . unScore

after :: Score a -> Score a -> Score a
after a b =
  case (getEnvA a, getEnvB b) of
    (Just r, Just l) -> a <> delay (r - l) b
    _ -> undefined

note :: a -> Score a
note a = Score $ leaf (Envelope (Just 0) (Just 1)) a

toMusic :: Score a -> Music a
toMusic
  = fromMaybe (rest 0)
  . foldDUAL
      (\(Matrix m) a ->
          let V3 x y _ = m !* V3 0 1 1
          in rest x :+: E.note y a)
      (rest 0)
      (foldr1 (:=:))
      (const id)
      (const id)
  . unScore


par :: Score a -> Score a -> Score a
par = (<>)


withZeroTime :: Score a -> Score a
withZeroTime = Score . mapU (\(Envelope x _) -> Envelope x $ Just 0) . unScore

parL :: Score a -> Score a -> Score a
parL x = par x . withZeroTime


bar :: Rhythm a -> Score a
bar = asBars 1


asBars :: Rational -> Rhythm a -> Score a
asBars r
  = foldMap (\(i, a) -> delay (getOffset i * r) $ scale (getDuration i * r) $ note a)
  . intervals


playScore :: Score Pitch -> IO ()
playScore
  = playDev 2
  . toMusic

data MkScore a
  = Note a
  | Scale Rational (MkScore a)
  | Delay Rational (MkScore a)
  | After (MkScore a) (MkScore a)
  deriving Show

mkScore :: MkScore a -> Score a
mkScore (Note a) = note a
mkScore (Scale a b) = scale a $ mkScore b
mkScore (Delay a b) = delay a $ mkScore b
mkScore (After a b) = after (mkScore a) $ mkScore b

instance Arbitrary a => Arbitrary (MkScore a) where
  arbitrary = sized $ \n ->
    case n <= 1 of
      True -> Note <$> arbitrary
      False -> oneof
        [ Note <$> arbitrary
        , Scale <$> arbitrary <*> Q.scale (subtract 1) arbitrary
        , Delay <$> arbitrary <*> Q.scale (subtract 1) arbitrary
        , After <$> Q.scale (`div` 2) arbitrary <*> Q.scale (`div` 2) arbitrary
        ]
  shrink (Note a) = Note <$> shrink a
  shrink (Scale a b) = mconcat
    [ Scale <$> shrink a <*> pure b
    , Scale <$> pure a <*> shrink b
    , pure b
    ]
  shrink (Delay a b) = mconcat
    [ Delay <$> shrink a <*> pure b
    , Delay <$> pure a <*> shrink b
    , pure b
    ]
  shrink (After a b) = mconcat
    [ After <$> shrink a <*> pure b
    , After <$> pure a <*> shrink b
    , pure a
    , pure b
    ]

main :: IO ()
main = hspec $ do
  let test = ((===) `on` (toMusic . mkScore @Int))
  prop "delay/after" $ \d a b ->
    test
      (Delay d $ After a b)
      (After (Delay d a) b)

  prop "after assoc" $ \a b c ->
    test
      (After (After a b) c)
      (After a (After b c))

  prop "delay scale" $ \d s a ->
    test
      (Scale s (Delay d a))
      (Delay (d * s) (Scale s a))



