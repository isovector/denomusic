{-# LANGUAGE Arrows          #-}
{-# LANGUAGE PatternSynonyms #-}

module FRP where

import Data.Set qualified as S
import Data.Set (Set)
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Bool
import Data.Coerce
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid
import Data.Ratio (denominator)
import Data.Semigroup qualified as S
import Prelude hiding (id, (.))


type Time = Rational

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

data Signal m a = Ord m => Signal
  { clock  :: Clock
  , sample :: Time -> Writer (Set m) a
  }

deriving stock instance Functor (Signal m)

instance Ord m => Applicative (Signal m) where
  pure = Signal mempty . pure . pure
  liftA2 f (Signal c1 a) (Signal c2 b) =
    Signal (c1 <> c2) $ liftA2 (liftA2 f) a b



newtype SF m a b = SF { runSF :: Signal m a -> Signal m b }
  deriving (Functor, Applicative) via WrappedArrow (SF m) a

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

emit :: SF m (Event m) ()
emit = arr (fmap S.singleton) >>> emitMany

emitMany :: SF m (Event (Set m)) ()
emitMany = SF $ \(Signal clk s) ->
  Signal clk $ \t -> do
    s t >>= \case
      Event x -> tell x
      NoEvent -> pure ()

play :: SF m (Event (Time, m)) (Event ())
play = proc e -> do
  emit -< fmap snd e
  arr void <<< move -< e


-- TODO(sandy): unwise?
censor :: SF m a (Event (Set m))
censor = SF $ \(Signal clk s) -> Signal clk $ \t -> do
  let (_, mus) = runWriter $ s t
  pure $
    case null mus of
      True -> NoEvent
      False -> Event mus


playBefore :: SF m (Event (Time, m)) (Event ())
playBefore = proc evs -> do
  delayed <- move -< fmap (negate *** id) evs
  emit -< delayed
  returnA -< void delayed

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

foldEvs :: (Time -> Writer x (Event a)) -> [Time] -> [(Time, a)]
foldEvs s ts = mapMaybe sequenceA $ zip ts $ fmap (eventToMaybe . fst . runWriter . s) ts

hold :: a -> SF m (Event a) a
hold a0 = SF $ \(Signal clk s) -> do
  let xs = foldEvs s $ getClock clk
  Signal clk $ \t -> do
    _ <- s t
    pure $ S.getLast $ S.sconcat $ coerce $ a0 :| fmap snd (takeWhile ((<= t) . fst) xs)

offset :: Time -> SF m a a
offset dt = SF $ \(Signal clk s) ->
  Signal (coerce (fmap @[] (+ dt)) clk) $ s . subtract dt

time :: SF m x Time
time = sf mempty const

played :: (m -> Bool) -> SF m x (Event m)
played f = SF $ \(Signal clk s) -> do
  let sampled = do
        t <- getClock clk
        let mm = listToMaybe $ filter f $ S.toList $ snd $ runWriter $ s t
        Just m <- pure mm
        pure (t, m)
  Signal clk $ \t -> pure $ MkEvent $ lookup t $ take 1 $ dropWhile ((< t) . fst) sampled


data Observation a = Observation
  { o_time :: Time
  , o_output :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

observe :: Ord m => SF m () a -> [Observation (Set m, a)]
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
accum a0 = SF $ \(Signal clk s) -> do
  let xs = scanl (\(_, a) (t, f) -> (t, f a)) (0, a0) $ foldEvs s $ getClock clk
  Signal clk $ \t -> do
    _ <- s t
    let ev = fst $ runWriter $ s t
    pure $ (S.getLast $ S.sconcat $ coerce $ a0 :| fmap snd (takeWhile ((<= t) . fst) xs)) <$ ev


onlyEvery :: Int -> SF m (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0


--------------------------------------------------------------------------------

main :: IO ()
main = print $ take 10 $ filter (not . null . fst . o_output) $ takeWhile ((<= 100) . o_time) $ observe $ test

test :: SF Char () ()
test = proc _ -> do
  x <- every 1 'x' -< ()
  y <- onlyEvery 2 -< x

  emit -< x
  emit -< 'y' <$ y

  z <- onlyEvery 2 <<< played (== 'y') -< ()
  emit -< 'z' <$ z

  returnA -< ()


