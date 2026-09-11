{-# LANGUAGE Arrows          #-}
{-# LANGUAGE PatternSynonyms #-}

module FRP where

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
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Generics
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


data Music = Music {}
  deriving stock (Eq, Ord, Show)

data Signal a = Signal
  { clock  :: Clock
  , sample :: Time -> Writer (Set Music) a
  }
  deriving stock (Functor, Generic, Generic1)
  deriving Applicative via Generically1 Signal

newtype SF a b = SF { runSF :: Signal a -> Signal b }

instance Category SF where
  id = SF id
  SF g . SF f = SF (g . f)

instance Arrow SF where
  arr = SF . fmap
  SF f *** SF g = SF $ \sb -> liftA2 (,) (f $ fmap fst sb) (g $ fmap snd sb)
  -- potential improvement?
  -- SF f *** SF g = SF $ \s -> do
  --   let clkf = clock $ f $ fmap fst s
  --       clkg = clock $ g $ fmap snd s

  --   Signal (clock s <> clkf <> clkg) $ \t -> do
  --     (a, b) <- sample s t
  --     a' <- sample (f $ Signal clkf $ const $ pure a) t
  --     b' <- sample (g $ Signal clkg $ const $ pure b) t
  --     pure (a', b')


sf :: Clock -> (Time -> a -> b) -> SF a b
sf clk' f = SF $ \(Signal clk s) ->
  Signal (clk <> clk') $ \t -> do
    a <- s t
    pure $ f t a



every :: Time -> a -> SF x (Event a)
every dur a = sf (Clock $ iterate (+ dur) 0) $ \t _ ->
  case denominator (t / dur) == 1 of
    True -> Event a
    False -> NoEvent

now :: a -> SF x (Event a)
now a = sf (Clock [0]) $ \t _ ->
  case t == 0 of
    True -> Event a
    False -> NoEvent

emit :: SF (Event Music) ()
emit = arr (fmap S.singleton) >>> emitMany

emitMany :: SF (Event (Set Music)) ()
emitMany = SF $ \(Signal clk s) ->
  Signal clk $ \t -> do
    s t >>= \case
      Event x -> tell x
      NoEvent -> pure ()

play :: SF (Event (Time, Music)) (Event ())
play = proc e -> do
  emit -< fmap snd e
  arr void <<< delaySF -< e


-- TODO(sandy): unwise?
censor :: SF a (Event (Set Music))
censor = SF $ \(Signal clk s) -> Signal clk $ \t -> do
  let (_, mus) = runWriter $ s t
  pure $
    case null mus of
      True -> NoEvent
      False -> Event mus

playBefore :: SF (Event (Time, Music)) (Event ())
playBefore = proc evs -> do
  delayed <- delaySF -< fmap (negate *** id) evs
  emit -< delayed
  returnA -< void delayed

delaySF :: SF (Event (Time, a)) (Event a)
delaySF = SF $ \(Signal clk s) -> do
  let sampled = do
        t <- getClock clk
        let (ev, _) = runWriter $ s t
        Event (dt, a) <- pure ev
        pure (t + dt, a)
  Signal (clk <> Clock (fmap fst sampled)) $ \t -> do
     _ <- s t
     pure $ maybe NoEvent Event $ lookup t sampled

foldEvs :: (Time -> Writer x (Event a)) -> [Time] -> [(Time, a)]
foldEvs s ts = mapMaybe sequenceA $ zip ts $ fmap (eventToMaybe . fst . runWriter . s) ts

hold :: a -> SF (Event a) a
hold a0 = SF $ \(Signal clk s) -> do
  let xs = foldEvs s $ getClock clk
  Signal clk $ \t -> do
    _ <- s t
    pure $ S.getLast $ S.sconcat $ coerce $ a0 :| fmap snd (takeWhile ((<= t) . fst) xs)

delay :: Time -> SF a a
delay dt = SF $ \(Signal clk s) ->
  Signal (coerce (fmap @[] (+ dt)) clk) $ s . subtract dt

time :: SF x Time
time = sf mempty const


data Observation a = Observation
  { o_time :: Time
  , o_output :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

observe :: SF () a -> [Observation (Set Music, a)]
observe (SF f) = do
  let Signal (Clock clk) s = f $ pure ()
  t <- clk
  let (a, stuff) = runWriter (s t)
  pure $ Observation t (stuff, a)


gate :: Bool -> Event a -> Event a
gate False _ = NoEvent
gate True e = e


afterNext :: SF (Event what, Event when) (Event (what, when))
afterNext = proc (ewhat, ewhen) -> do
  ewhen' <- delay 0.000000001 -< ewhen
  mwhat <- hold Nothing -< asum [fmap Just ewhat, Nothing <$ ewhen']
  returnA -< ewhen >>= \when -> MkEvent $ fmap (, when) mwhat

notYet :: SF (Event a) (Event a)
notYet = sf (Clock [0]) $ \t a ->
  case t <= 0 of
    True -> NoEvent
    False -> a

once :: SF (Event a) (Event a)
once = takeEvents 1

takeEvents :: Int -> SF (Event a) (Event a)
takeEvents n = proc ev -> do
  ev' <- accum (0, NoEvent)
      -< ev <&> \a -> \(n', _) ->
          bool (n', NoEvent) (n' + 1, Event a) $ n' < n
  returnA -< ev' >>= snd


dropEvents :: Int -> SF (Event a) (Event a)
dropEvents n = proc ev -> do
  ev' <- accum (n, NoEvent)
      -< ev <&> \a -> \(n', _) ->
          bool (n', Event a) (n' - 1, NoEvent) $ n' > 0
  returnA -< ev' >>= snd

accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = SF $ \(Signal clk s) -> do
  let xs = scanl (\(_, a) (t, f) -> (t, f a)) (0, a0) $ foldEvs s $ getClock clk
  Signal clk $ \t -> do
    _ <- s t
    let ev = fst $ runWriter $ s t
    pure $ (S.getLast $ S.sconcat $ coerce $ a0 :| fmap snd (takeWhile ((<= t) . fst) xs)) <$ ev


onlyEvery :: Int -> SF (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0


--------------------------------------------------------------------------------

main :: IO ()
main = print $ take 10 $ filter (not . null . fst . o_output) $ takeWhile ((<= 100) . o_time) $ observe $ test

test :: SF () ()
test = proc _ -> do
  t <- dropEvents 4 <<< every 1 () -< ()
  emit -< Music <$ t
  returnA -< ()


