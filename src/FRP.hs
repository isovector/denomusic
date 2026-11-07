{-# LANGUAGE BlockArguments  #-}
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
hold a0
  = SF
  $ mkSteps a0
  . scanSteps (\a -> event a id) a0


fhold :: a -> SF (Event a) a
fhold a0 = SF $ \s -> do
  case eventsTerminating s of
    [] -> Signal (const a0) mempty
    ((t, a) : as) ->
      mkSteps a $ zip (t : fmap fst as) $ fmap snd as <> [a0]


every :: Time -> a -> SF x (Event a)
every dur a = discrete $ zip (iterate (+ dur) 0) $ repeat a

rhythm :: [Time] -> a -> SF x (Event a)
rhythm durs a = discrete $ zip (scanl (+) 0 $ cycle durs) $ repeat a

at :: Time -> a -> SF x (Event a)
at t a = discrete $ pure (t, a)

at' :: Time -> SF i (Event i)
at' t = proc i -> do
  e <- at t () -< i
  returnA -< i <$ e

-- | Stretch time by the given amount. This is probably not what you want!
-- Instead, you likely want 'stretch'!
warpSignal :: Rational -> SF a a
warpSignal (fromRational -> r) = SF $ invmapTime (* r) (/ r)

stretch :: Rational -> SF a b -> SF a b
stretch t sf = warpSignal (recip t) >>> sf >>> warpSignal t

stretchS :: Rational -> Signal a -> Signal a
stretchS (fromRational -> r) = invmapTime (* r) (/ r)

now :: a -> SF x (Event a)
now = at 0

never :: SF x (Event a)
never = discrete []

offset :: Time -> SF a a
offset dt = SF $ \input ->
  unionClock input $ invmapTime (+ dt) (subtract dt) input

localTime :: SF x Time
localTime = SF $ const $ Signal id mempty

replace :: [a] -> SF (Event b) (Event (b, a))
replace = fmap join . scanE \as b ->
  case as of
    []  -> ([], NoEvent)
    (a : as') -> (as', Event (b, a))

partitionEvents :: (a -> Either b c) -> SF (Event a) (Event b, Event c)
partitionEvents f = proc eva -> do
  evb <- mapMaybeE (either Just (const Nothing) . f) -< eva
  evc <- mapMaybeE (either (const Nothing) Just . f) -< eva
  returnA -< (evb, evc)


filterE :: Arrow p => (a -> Bool) -> p (Event a) (Event a)
filterE f = arr $ \ea -> ea >>= \a ->
  case f a of
    True -> pure a
    False -> NoEvent

filterTimeE :: (Time -> Bool) -> SF (Event a) (Event a)
filterTimeE f = proc e -> do
  t <- localTime -< e
  fmap (fmap snd) $ filterE (f . fst) -< fmap (t, ) e

mapMaybeE :: Arrow p => (a -> Maybe b) -> p (Event a) (Event b)
mapMaybeE f = arr $ (>>= maybe NoEvent Event . f)


gate :: Bool -> Event a -> Event a
gate False _ = NoEvent
gate True e = e

notYet :: SF (Event a) (Event a)
notYet = filterTimeE (> 0)

once :: SF (Event a) (Event a)
once = takeE 1

takeE :: Int -> SF (Event a) (Event a)
takeE = fmap join . scanE \n a ->
  case n <= 0 of
    True -> (0, NoEvent)
    False -> (n - 1, Event a)

dropE :: Int -> SF (Event a) (Event a)
dropE = fmap join . scanE \n a ->
  case n <= 0 of
    True -> (0, Event a)
    False -> (n - 1, NoEvent)

accum :: a -> SF (Event (a -> a)) (Event a)
accum = scanE $ \a f -> let fa = f a in (fa, fa)

onlyEvery :: Int -> SF (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0


epsilon :: Time
epsilon = 0.0000000000001

diffs :: Num a => SF (Event a) (Event (a -> a))
diffs = proc e -> do
  aprev <- hold 0 <<< offset epsilon -< e
  returnA -< fmap (\x anew -> anew + (x - aprev)) e


attractor :: Fractional a => Time -> SF (Event a, Event a) (Event a)
attractor (Time dur) = proc (ea, eatt) -> do
  t <- localTime -< ()
  (t_at, a_attr) <- fhold (99999, 0) <<< offset epsilon -< fmap (t, ) eatt
  let (Time dt) = t_at - t
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

msumE :: Monoid a => SF (Event a) a
msumE = foldMapE id

foldMapE :: Monoid m => (a -> m) -> SF (Event a) m
foldMapE f = arr (fmap $ (<>) . f) >>> accum mempty >>> hold mempty

