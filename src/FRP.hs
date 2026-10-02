{-# OPTIONS_GHC -Wno-orphans #-}

module FRP
  ( module Control.Arrow
  , module FRP
  , module FRP.Types
  , module FRP.Primitives
  , Alternative (..)
  , Interval(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Cont
import Data.Bool
import Data.Coerce
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.Ratio
import FRP.Primitives
import FRP.Types
import Prelude hiding (id, (.))


hold :: a -> SF (Event a) a
hold a0 = SF $ mkSteps a0 . events


fhold :: a -> SF (Event a) a
fhold a0 = SF $ \s -> do
  case eventsTerminating s of
    [] -> Signal (const a0) mempty
    ((t, a) : as) ->
      mkSteps a $ zip (t : fmap fst as) $ fmap snd as <> [a0]


every :: Time -> a -> SF x (Event a)
every dur a = discrete $ zip (iterate (+ dur) 0) $ repeat a

at :: Time -> a -> SF x (Event a)
at t' a = discrete $ pure (t', a)

-- | Stretch time by the given amount.
stretch :: Rational -> SF a a
stretch r = SF $ invmapTime (* r) (/ r)

now :: a -> SF x (Event a)
now = at 0

offset :: Time -> SF a a
offset dt = SF $ invmapTime (+ dt) (subtract dt)

localTime :: SF x Time
localTime = SF $ const $ Signal id mempty

replace :: [a] -> SF (Event b) (Event (b, a))
replace as = ev2ev $ \bs -> zipWith (\(t, b) a -> (t, (b, a))) bs as

partitionEvents :: (a -> Either b c) -> SF (Event a) (Event b, Event c)
partitionEvents f = proc eva -> do
  evb <- mapMaybeE (either Just (const Nothing) . f) -< eva
  evc <- mapMaybeE (either (const Nothing) Just . f) -< eva
  returnA -< (evb, evc)


filterE :: (a -> Bool) -> SF (Event a) (Event a)
filterE f = ev2ev $ filter (f . snd)

filterTimeE :: (Time -> Bool) -> SF (Event a) (Event a)
filterTimeE f = ev2ev $ filter (f . fst)

mapMaybeE :: (a -> Maybe b) -> SF (Event a) (Event b)
mapMaybeE = ev2ev . mapMaybe . traverse


gate :: Bool -> Event a -> Event a
gate False _ = NoEvent
gate True e = e

notYet :: SF (Event a) (Event a)
notYet = filterTimeE (> 0)

once :: SF (Event a) (Event a)
once = takeE 1

takeE :: Int -> SF (Event a) (Event a)
takeE = ev2ev . take

dropE :: Int -> SF (Event a) (Event a)
dropE = ev2ev . drop

accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = ev2ev $ drop 1 . scanl (\(_, a) (t', f) -> (t', f a)) (undefined, a0)

onlyEvery :: Int -> SF (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0

newtype Seq i o a = Seq
  { unSeq :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)


toSeq :: SF i (Event o, Event a) -> Seq i (Event o) a
toSeq = Seq . cont . switch

switchSeq :: Seq i o a -> (a -> SF i o) -> SF i o
switchSeq = runCont . unSeq

getSeq :: Seq i (Event o) a -> SF i (Event o)
getSeq = flip switchSeq $ const $ arr $ const NoEvent

rest :: Time -> Seq i (Event a) ()
rest t = toSeq $ proc i -> do
  e <- at t () -< i
  returnA -< (NoEvent, e)

hit :: Time -> a -> Seq i (Event a) ()
hit t a = toSeq $ proc i -> do
  n <- now a -< i
  e <- at t () -< i
  returnA -< (n, e)

epsilon :: Time
epsilon = 0.0000000000001

diffs :: Num a => SF (Event a) (Event (a -> a))
diffs = proc e -> do
  aprev <- hold 0 <<< offset epsilon -< e
  returnA -< fmap (\x anew -> anew + (x - aprev)) e


attractor :: Fractional a => Time -> SF (Event a, Event a) (Event a)
attractor dur = proc (ea, eatt) -> do
  t <- localTime -< ()
  (t_at, a_attr) <- fhold (99999, 0) <<< offset epsilon -< fmap (t, ) eatt
  let dt = t_at - t
  eda <- diffs -< ea
  accum 0 -< eda <&> \da ->
    case dt >= 0 && dt <= dur of
      True -> lerp (fromRational $ dt / dur) a_attr
      False -> da

lerp :: (Real b, Fractional a) => b -> a -> a -> a
lerp x lo hi = (1 - realToFrac x) * lo + realToFrac x * hi


discreteTime :: Time -> SF x (Event Time)
discreteTime rate = proc _ -> do
  t <- localTime -< ()
  e <- every rate () -< ()
  returnA -< t <$ e


-- | Continuously lerp between events.
smoothly
    :: (Applicative f, Integral a, Fractional b)
    => SF (Event (f a)) (f b)
smoothly = proc es -> do
  t <- localTime -< ()
  let ets = fmap (\e -> (t, fmap fromIntegral e)) es
  rec
    ~e0@(t0, fa0) <- hold  e' -<< ets
    ~e'@(t', fa') <- fhold e0 -<< ets
  returnA -<
    case t' - t0 == 0 of
      True -> fa0
      False ->
        lerp ((t - t0) / (t' - t0))
          <$> fa0
          <*> fa'


smoothly1 :: forall a b. (Integral a, Fractional b) => SF (Event a) b
smoothly1 = coerce $ smoothly @Identity @a @b


-- | Compute the 'Event's that would have to happen in between the existing
-- event stream in order to give continuous enumerable motion from one to the
-- next.
inBetween :: (Enum a, Ord a) => SF (Event a) (Event a)
inBetween = evByEv $ \(t1, a1) (t2, a2) -> do
  let as =
        zip [1..] $ case compare a1 a2 of
          LT -> enumFromTo (succ a1) (pred a2)
          GT -> reverse $ enumFromTo (succ a2) (pred a1)
          EQ -> mempty
  let dt = (t2 - t1) / (fromIntegral (length as) + 1)
  (ix, a) <- as
  pure (t1 + fromIntegral @Int ix * dt, a)


-- | Keep the existing events as 'Right's; compute their 'inBetween's as
-- 'Left's.
continuation :: (Enum a, Ord a) => SF (Event a) (Event (Either a a))
continuation = proc es -> do
  es' <- inBetween -< es
  returnA -< fmap Right es <> fmap Left es'

