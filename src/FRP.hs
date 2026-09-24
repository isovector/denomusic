{-# OPTIONS_GHC -Wno-orphans #-}

module FRP
  ( module Control.Arrow
  , Alternative (..)
  , module FRP
  , module FRP.Types
  , Interval(..)
  ) where

import Control.Monad.Cont
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Exception (evaluate)
import Control.Monad (join)
import Data.Align
import Data.Bool
import Data.Coerce
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.These
import FRP.Types
import Prelude hiding (id, (.))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)



sf :: Clock -> (Time -> a -> b) -> SF a b
sf clk' f = SF $ \(Signal clk s) ->
  Signal (clk <> clk') $ \t -> f t (s t)


-- downbeat :: SF (Event Beat) (Event ())
-- downbeat = fmap void $ filterEvents (== 0)

-- upbeat :: SF (Event Beat) (Event ())
-- upbeat = proc ev -> do
--   y <- fhold (-1) -< ev
--   returnA -< void $ ev >> bool NoEvent (Event ()) (y == 0)

-- | The 'Time's must be monotonically increasing.
discrete :: [(Time, a)] -> SF x (Event a)
discrete ts =
  sf (Clock $ fmap fst ts) $ \t _ ->
    MkEvent $ lookup t ts

every :: Time -> a -> SF x (Event a)
every dur a = sf (Clock $ iterate (+ dur) 0) $ \t _ ->
  case denominator (t / dur) == 1 of
    True -> Event a
    False -> NoEvent

at :: Time -> a -> SF x (Event a)
at t' a = sf (Clock [t']) $ \t _ ->
  case t == t' of
    True -> Event a
    False -> NoEvent

-- | Stretch time by the given amount.
stretch :: Rational -> SF a a
stretch r = SF $ \(Signal clk s) ->
  Signal (coerce (fmap @[] (* r)) clk) $ s . (/ r)

now :: a -> SF x (Event a)
now = at 0

-- | Observe whether a computation would diverge, and if so, return 'Nothing'
-- instead. This can be used to guard otherwise-sketchy combinators which need
-- to fold over infinite event streams.
--
-- This is implemented by terminating after 10ms of trying.
terminating :: a -> Maybe a
terminating = unsafePerformIO . timeout 10_000 . evaluate

switchBy :: (b -> b -> b) -> SF a (b, Event c) -> (c -> SF a b) -> SF a b
switchBy merge (SF f) k = SF $ \sig0 -> do
  let sig1 = f sig0
      sig1b = fmap fst sig1
  case join $ terminating $ listToMaybe $ mapMaybe sequenceA $ signalEvs $ fmap snd sig1 of
    Nothing -> sig1b
    Just (t0, c) -> do
      let sig2 = runSF (offset t0 <<< k c <<< offset (- t0)) sig0
      Signal (clock sig1 <> clock sig2) $ \t ->
        case compare t0 t of
          GT -> sample sig1b t
          LT -> sample sig2 t
          EQ -> merge (sample sig1b t) (sample sig2 t)



-- | Construct an 'SF' by folding over an input event stream.
evSF :: ([(Time, Maybe a)] -> Time -> b) -> SF (Event a) b
evSF f = SF $ \sig@(Signal clk _) -> do
  let xs = signalEvs sig
  Signal clk $ \t -> f xs t


-- | Hold the value of the most recent value of an 'Event'.
hold :: a -> SF (Event a) a
hold a0 = evSF $ \xs t ->
  fromMaybe a0
    $ getLast
    $ foldMap (Last . snd)
    $ bounded t xs

-- | Hold the value of the next (not yet occurred!) value of an 'Event'.
fhold :: a -> SF (Event a) a
fhold a0 = evSF $ \xs t ->
  fromMaybe a0
    $ getFirst
    $ foldMap (First . snd)
    $ dropWhile ((<= t) . fst) xs

offset :: Time -> SF a a
offset dt = SF $ \(Signal clk s) ->
  Signal (coerce (fmap @[] (+ dt)) clk) $ s . subtract dt

localTime :: SF x Time
localTime = sf mempty const

bounded :: Time -> [(Time, Maybe a)] -> [(Time, Maybe a)]
bounded t = takeWhile ((<= t) . fst)

values :: Time -> [(Time, Maybe a)] -> [(Time, a)]
values t = mapMaybe sequenceA . bounded t

replace :: [a] -> SF (Event b) (Event (b, a), Event b)
replace as = evSF $ \xs t -> do
  let bs = values t xs
  fromMaybe (NoEvent, NoEvent)
    $ lookup t
    $ flip mapMaybe (align (take (length bs) as) bs) $
        \case
          This _ -> Nothing
          That (t', b) -> Just (t', (NoEvent, Event b))
          These a (t', b) -> Just (t', (Event (b, a), NoEvent))

-- partitionEvents :: (a -> Either b c) -> SF (Event a) (Event b, Event c)
-- partitionEvents f = evSF $ \as t -> do
--   let (bs, cs) = partitionEithers $ fmap (\(t', a) -> bimap (t',) (t',) $ f a) as
--       go :: [(Time, x)] -> Event x
--       go = maybe NoEvent Event . join . terminating . lookup t
--   (go bs, go cs)

filterEvents :: (a -> Bool) -> SF (Event a) (Event a)
filterEvents f = evSF $ \as t ->
  MkEvent
    $ lookup t
    $ filter (f . snd)
    $ values t as


gate :: Bool -> Event a -> Event a
gate False _ = NoEvent
gate True e = e


-- afterNext :: SF (Event what, Event when) (Event (what, when))
-- afterNext = proc (ewhat, ewhen) -> do
--   ewhen' <- offset 0.000000001 -< ewhen
--   mwhat <- hold Nothing -< asum [fmap Just ewhat, Nothing <$ ewhen']
--   returnA -< ewhen >>= \when -> MkEvent $ fmap (, when) mwhat

notYet :: SF (Event a) (Event a)
notYet = sf (Clock [0]) $ \t a ->
  case t <= 0 of
    True -> NoEvent
    False -> a

once :: SF (Event a) (Event a)
once = takeEvents 1

takeEvents :: Int -> SF (Event a) (Event a)
takeEvents n = evSF $ \evs t -> MkEvent $ lookup t $ take n $ values t evs


-- dropEvents :: Int -> SF (Event a) (Event a)
-- dropEvents n = evSF $ \evs t -> MkEvent $ join $ terminating $ lookup t $ drop n evs

accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = evSF $ \evs t -> do
  let xs = scanl (\(_, a) (t', mf) ->
            case mf of
              Just f -> (t', f a)
              Nothing -> (t', a)) (0, a0) evs
  MkEvent $ lookup t xs

foldE :: Monoid a => SF (Event a) (Event a)
foldE = accum mempty . arr (fmap (<>))

onlyEvery :: Int -> SF (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0


newtype Seq i o a = Seq
  { unSeq :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)

toSeqBy :: (o -> o -> o) -> SF i (o, Event a) -> Seq i o a
toSeqBy f = Seq . cont . switchBy f

toSeq :: SF i (Event o, Event a) -> Seq i (Event o) a
toSeq = toSeqBy (flip (<|>))

switchSeq :: Seq i o a -> (a -> SF i o) -> SF i o
switchSeq = runCont . unSeq

getSeq :: Seq i (Event o) a -> SF i (Event o)
getSeq = flip switchSeq $ const $ arr $ const NoEvent

rest :: Time -> Seq i (Event a) ()
rest t = toSeq $ proc i -> do
  e <- at t () -< i
  returnA -< (NoEvent, e)

pulse :: a -> Seq i (Event a) ()
pulse a = toSeq $ proc i -> do
  n <- now a -< i
  returnA -< (n, void n)

hit :: Time -> a -> Seq i (Event a) ()
hit t a = do
  pulse a
  rest t

beat :: Time -> Priority -> Seq i (Event Beat) ()
beat t p = hit t $ Beat t p

