{-# OPTIONS_GHC -Wno-orphans #-}

module FRP
  ( module Control.Arrow
  , Alternative (..)
  , module FRP
  , Interval(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Exception (evaluate)
import Control.Monad.Writer (Writer, runWriter, tell, mapWriter)
import Data.Bool
import Data.Coerce
import Data.Functor
import Data.IntervalMap.FingerTree (Interval(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.MemoTrie
import Data.Monoid
import Data.Ratio
import Data.Semigroup qualified as S
import Data.Set (Set)
import Data.Set qualified as S
import Prelude hiding (id, (.))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)


type Time = Rational

instance HasTrie Rational where
  data Rational :->: x = RationalTrie (Integer :->: (Integer :->: x))
  trie f = RationalTrie $ trie $ trie . \n d -> f (n % d)
  untrie (RationalTrie x) = (\f r -> f (numerator r) (denominator r)) (untrie . untrie x)
  enumerate = error "no enumerate for Rational"


-- | A (possibly infinite) list of interesting times
newtype Clock = Clock { getClock :: [Time] }

-- | Merge two clocks, keeping them in ascending time order
instance Semigroup Clock where
  Clock [] <> Clock ys = Clock ys
  Clock (x : xs) <> Clock [] = Clock (x : xs)
  xx@(Clock (x : xs)) <> yy@(Clock (y : ys)) =
    case compare x y of
      LT -> Clock $ x : coerce (Clock xs <> yy)
      GT -> Clock $ y : coerce (xx <> Clock ys)
      EQ -> Clock $ x : coerce (Clock xs <> Clock ys)

instance Monoid Clock where
  mempty = Clock []

newtype Event a = MkEvent
  { eventToMaybe :: Maybe a
  }
  deriving stock (Foldable, Traversable)
  deriving newtype (Functor, Applicative, Monad, Eq, Ord, Show, Alternative)

{-# COMPLETE Event, NoEvent #-}
pattern Event :: a -> Event a
pattern Event a = MkEvent (Just a)

pattern NoEvent :: Event a
pattern NoEvent = MkEvent Nothing

instance Semigroup a => Semigroup (Event a) where
  NoEvent <> a = a
  Event a <> NoEvent = Event a
  Event a <> Event b = Event (a <> b)

instance Semigroup a => Monoid (Event a) where
  mempty = NoEvent

data Signal m a = Ord m => UnsafeSignal
  { clock  :: Clock
  , sample :: Time -> Writer (Set (Time, m)) a
  }

pattern Signal :: () => Ord m => Clock -> (Time -> Writer (Set (Time, m)) a) -> Signal m a
pattern Signal c f <- UnsafeSignal c f
  where
    Signal c f = UnsafeSignal c $ memo f
{-# COMPLETE Signal #-}

instance Functor (Signal m) where
  fmap f (Signal c g) = Signal c $ fmap (fmap f) g

instance Ord m => Applicative (Signal m) where
  pure = Signal mempty . pure . pure
  liftA2 f (Signal c1 a) (Signal c2 b) =
    Signal (c1 <> c2) $ liftA2 (liftA2 f) a b

newtype SF m a b = SF { runSF :: Signal m a -> Signal m b }
  deriving (Functor, Applicative) via WrappedArrow (SF m) a
  deriving (Semigroup, Monoid) via Ap (SF m a) b

instance Category (SF m) where
  id = SF id
  SF g . SF f = SF (g . f)

instance Arrow (SF m) where
  arr = SF . fmap
  SF f *** SF g = SF $ \sg@(Signal{}) ->
    liftA2 (,) (f $ fmap fst sg) (g $ fmap snd sg)


sf :: Clock -> (Time -> a -> b) -> SF m a b
sf clk' f = SF $ \(Signal clk s) ->
  Signal (clk <> clk') $ \t -> do
    a <- s t
    pure $ f t a


-- | The 'Time's must be monotonically increasing.
discrete :: [(Time, a)] -> SF m x (Event a)
discrete ts =
  sf (Clock $ fmap fst ts) $ \t _ ->
    MkEvent $ lookup t ts



every :: Time -> a -> SF m x (Event a)
every dur a = sf (Clock $ iterate (+ dur) 0) $ \t _ ->
  case denominator (t / dur) == 1 of
    True -> Event a
    False -> NoEvent

now :: a -> SF m x (Event a)
now a = sf (Clock [0]) $ \t _ ->
  case t == 0 of
    True -> Event a
    False -> NoEvent


-- | Stretch time by the given amount, without changing the duration of emitted
-- notes.
stretch :: Rational -> SF m a a
stretch r = SF $ \(Signal clk s) ->
  Signal (coerce (fmap @[] (* r)) clk) $ s . (/ r)

-- | Stretch time by the given amount, including the duration of already
-- emitted notes.
magnify :: Rational -> SF m a b -> SF m a b
magnify r f = SF $ \sig@Signal{} -> do
  let (Signal clk s) = runSF (stretch r <<< f) sig
  Signal clk $ mapWriter (fmap $ S.map $ first (* r)) . s

at :: Time -> a -> SF m x (Event a)
at t a = offset t <<< now a

emit :: SF m (Event (Time, m)) ()
emit = arr (fmap S.singleton) >>> emitMany

emitMany :: SF m (Event (Set (Time, m))) ()
emitMany = SF $ \(Signal clk s) ->
  Signal clk $ \t -> do
    s t >>= \case
      Event x -> tell x
      NoEvent -> pure ()

play :: SF m (Event (Time, m)) (Event ())
play = proc e -> do
  emit -< e
  arr void <<< move -< e

rest :: SF m (Event Time) (Event ())
rest = arr void <<< move <<< arr (fmap (, ()))

-- | Observe whether a computation would diverge, and if so, return 'Nothing'
-- instead. This can be used to guard otherwise-sketchy combinators which need
-- to fold over infinite event streams.
--
-- This is impolemented by terminating after 10ms of trying.
terminating :: a -> Maybe a
terminating = unsafePerformIO . timeout 10_000 . evaluate

-- TODO(sandy): What would fswitch do? Run the first SF until the event in the
-- second would trigger?
switch :: SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch (SF f) k = SF $ \sig0@Signal{} -> do
  let sig1 = f sig0
      sig1b = fmap fst sig1
  case listToMaybe $ signalEvs $ fmap snd sig1 of
    Nothing -> sig1b
    Just (t0, c) -> do
      let sig2 = runSF (offset t0 <<< k c <<< offset (- t0)) sig0
      Signal (clock sig1 <> clock sig2) $ \t ->
        flip sample t $ bool sig1b sig2 $ t >= t0


-- playBefore :: SF m (Event (Time, m)) (Event ())
-- playBefore = proc evs -> do
--   delayed <- move -< fmap (negate *** id) evs
--   emit -< delayed
--   returnA -< void delayed

move :: SF m (Event (Time, a)) (Event a)
move = SF $ \(Signal clk s) -> do
  let sampled = do
        t <- getClock clk
        let (ev, _) = runWriter $ s t
        Event (dt, a) <- pure ev
        pure (t + dt, a)
  Signal (clk <> Clock (fmap fst sampled)) $ \t -> do
     _ <- s t
     pure $ maybe NoEvent Event $ lookup t $ takeWhile ((<= t) . fst) sampled


-- | Fold a 'Signal' into its event stream.
signalEvs :: Signal m (Event a) -> [(Time, a)]
signalEvs (Signal (Clock ts) f) = mapMaybe sequenceA $ zip ts $ fmap (eventToMaybe . fst . runWriter . f) ts


-- | Construct an 'SF' by folding over an input event stream.
evSF :: ([(Time, a)] -> Time -> b) -> SF m (Event a) b
evSF f = SF $ \sig@(Signal clk s) -> do
  let xs = signalEvs sig
  Signal clk $ \t -> do
    _ <- s t
    pure $ f xs t


-- | Hold the value of the most recent value of an 'Event'.
hold :: a -> SF m (Event a) a
hold a0 = evSF $ \xs t -> fromMaybe a0 $ terminating $ S.getLast $ S.sconcat $ coerce $ a0 :| fmap snd (takeWhile ((<= t) . fst) xs)

-- | Hold the value of the next (not yet occurred!) value of an 'Event'.
fhold :: a -> SF m (Event a) a
fhold a0 = evSF $ \xs t ->
  maybe a0 snd $ listToMaybe (dropWhile ((<= t) . fst) xs) >>= terminating

offset :: Time -> SF m a a
offset dt = SF $ \(Signal clk s) ->
  Signal (coerce (fmap @[] (+ dt)) clk) $ s . subtract dt

localTime :: SF m x Time
localTime = sf mempty const

played :: (m -> Bool) -> SF m x (Event (Time, m))
played f = SF $ \(Signal clk s) -> do
  let sampled = do
        t <- getClock clk
        let mm = listToMaybe $ filter (f . snd) $ S.toList $ snd $ runWriter $ s t
        Just m <- pure mm
        pure (t, m)
  Signal clk $ \t -> pure $ MkEvent $ lookup t $ take 1 $ dropWhile ((< t) . fst) sampled


data Observation a = Observation
  { o_time :: Time
  , o_output :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

observe :: Ord m => SF m () a -> [Observation (Set (Time, m), a)]
observe (SF f) = do
  let Signal (Clock clk) s = f $ pure ()
  t <- clk
  let (a, stuff) = runWriter (s t)
  pure $ Observation t (stuff, a)


gate :: Bool -> Event a -> Event a
gate False _ = NoEvent
gate True e = e


afterNext :: SF m (Event what, Event when) (Event (what, when))
afterNext = proc (ewhat, ewhen) -> do
  ewhen' <- offset 0.000000001 -< ewhen
  mwhat <- hold Nothing -< asum [fmap Just ewhat, Nothing <$ ewhen']
  returnA -< ewhen >>= \when -> MkEvent $ fmap (, when) mwhat

notYet :: SF m (Event a) (Event a)
notYet = sf (Clock [0]) $ \t a ->
  case t <= 0 of
    True -> NoEvent
    False -> a

once :: SF m (Event a) (Event a)
once = takeEvents 1

takeEvents :: Int -> SF m (Event a) (Event a)
takeEvents n = proc ev -> do
  ev' <- accum (0, NoEvent)
      -< ev <&> \a -> \(n', _) ->
          bool (n', NoEvent) (n' + 1, Event a) $ n' < n
  returnA -< ev' >>= snd


dropEvents :: Int -> SF m (Event a) (Event a)
dropEvents n = proc ev -> do
  ev' <- accum (n, NoEvent)
      -< ev <&> \a -> \(n', _) ->
          bool (n', Event a) (n' - 1, NoEvent) $ n' > 0
  returnA -< ev' >>= snd

accum :: a -> SF m (Event (a -> a)) (Event a)
accum a0 = evSF $ \evs t -> do
  let xs = scanl (\(_, a) (t', f) -> (t', f a)) (0, a0) evs
  let ev = lookup t $ takeWhile ((<= t) . fst) evs
  (S.getLast $ S.sconcat $ coerce $ a0 :| fmap snd (takeWhile ((<= t) . fst) xs)) <$ MkEvent ev


onlyEvery :: Int -> SF m (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0


export :: Ord m => (Rational, Rational) -> SF m () x -> [(Interval Rational, Set m)]
export (lo, hi) s
  = mapMaybe (\o -> do
      let t = o_time o
      (d, _) <- S.lookupMin $ o_output o
      pure (Interval t (t + d), S.map snd $ o_output o)
        )

      -- let t = o_time o
      --  in
  $ fmap (fmap fst)
  $ takeWhile ((<= hi) . o_time)
  $ dropWhile ((< lo) . o_time)
  $ observe s



