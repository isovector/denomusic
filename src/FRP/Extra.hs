module FRP.Extra where

import Data.Proxy (Proxy (..))
import GHC.TypeLits
import DenoMusic.Harmony
import DenoMusic.Types
import Data.Set qualified as S
import FRP
import FRP.Beat


partitionBeats :: Priority -> SF (Event Beat) (Event Beat, Event Beat)
partitionBeats p = proc b -> do
  strong <- filterE ((<= p) . stress) -< b
  weak   <- filterE ((> p) . stress)  -< b
  returnA -< (strong, weak)


setDuration :: Time -> Beat -> Beat
setDuration d (Beat _ p) = Beat d p

line' :: ((Time, T xs Int) -> a) -> [T xs Int] -> SF (Event Beat) (Event a)
line' f ts = proc e -> do
  e' <- replace ts -< e
  returnA -< fmap f $ fmap (first duration) e'


line
    :: (KnownNat y, Semigroup (T xs Int))
    => ((Time, T (y ': xs) Int) -> a)
    -> [T (x ': y ': xs) Int]
    -> SF (Chord x (y ': xs), Event Beat) (Event a)
line f ts = proc (ch, e) -> do
  e' <- replace ts -< e
  returnA -< fmap f $ fmap (duration *** chordTone ch) e'

chord
    :: forall x y xs a
     . ( KnownNat x
       , KnownNat y
       , Monoid (T xs Int)
       , Semigroup a
       )
    => ((Time, T (y ': xs) Int) -> a)
    -> SF (Chord x (y ': xs), Event Beat) (Event a)
chord f = proc (ch, e) -> do
  returnA -<
    mconcat $ do
      ix <- [0 .. fromIntegral $ natVal (Proxy @x)]
      pure $ fmap f $ fmap (\b -> (duration b, chordTone ch $ extend (ix :> Nil))) e


toNote
    :: Semigroup (T xs Int)
    => MetaScales xs PitchClass
    -> T xs Int
    -> (Time, T xs Int) -> Notes (Reg PitchClass)
toNote sc t0 (t, x) = Notes $
  S.singleton
    (t, elim sc (Reg 4 C) $ x <> t0)

type Chord x xs = (MetaScale x, T (x ': xs) Int)

chordTone
    :: (KnownNat y, Semigroup (T xs Int))
    => Chord x (y ': xs)
    -> T (x ': y ': xs) Int
    -> T (y ': xs) Int
chordTone (ms, t) t0 = kill ms (t <> t0)


