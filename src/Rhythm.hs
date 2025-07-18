{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE ViewPatterns       #-}

module Rhythm
( module Rhythm
, Bound (..)
, SF (..)
) where

import Debug.Trace
import Data.Ratio
import qualified Data.Map as M
import Data.Function.Step (SF(..), Bound(..), fromList)
import Data.Function.Step qualified as SF
import Control.Monad (ap, guard)
import Control.Arrow ((***), (&&&))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.Hspec
import Test.Hspec.QuickCheck

data Rhythm a
  = Empty
  | Full a
  | Interval (SF Rational (Rhythm a))
  | Par (Rhythm a) (Rhythm a)
  deriving stock (Show, Functor, Foldable, Traversable)

mu :: Rhythm a -> Rational -> [a]
mu _ n | n > 1 = []
mu _ n | n < 0 = []
mu Empty _ = []
mu (Full a) _ = pure a
-- mu (Interval (SF _ d)) 1 = mu d 1
mu (Interval as) n =
  let ((unbound -> left, unbound -> right), m) = findInterval n as
      width = right - left
   in
      case width of
        0 -> error $ show $ fst $ findInterval n as
        _ -> mu m $ (n - left) / width
mu (Par a b) n = mu a n <> mu b n


liftA2SF
  :: forall k a b c
   . (k ~ Rational)
  => ((Bound k, Bound k) -> a -> b -> c)
  -> SF k a
  -> SF k b
  -> SF k c
liftA2SF f (SF (M.toList -> as0) defa) (SF (M.toList -> bs0) defb) =
  SF (M.fromAscList $ go (as0 <> pure (Closed 1, defa)) (bs0 <> pure (Closed 1, defb))) (f (swapBound hi, Closed 1) defa defb)
  where
    hi = max
          (maximum $ fmap fst as0)
          (maximum $ fmap fst bs0)

    go :: [(Bound k, a)] -> [(Bound k, b)] -> [(Bound k, c)]
    go [] [] = []
    go [_] [] = []
    go [] [_] = []
    go ((i, a) : as@((i2, _) : _)) [] = (i, f (i, i2) a defb) : go as []
    go [] ((i, b) : bs@((i2, _) : _)) = (i, f (i, i2) defa b) : go [] bs
    go as@((ia, a) : as') bs@((ib, b) : bs') = do
      let bounds = (ia, ib)
      case compare ia ib of
        LT -> (ia, f bounds a b) : go as' bs
        -- TODO(sandy): wtf am i doing. need a smarter accumulator
        EQ -> (ia, f bounds a b) : go as' bs'
        GT -> (ib, f bounds a b) : go as bs'


unbound :: Bound a -> a
unbound (Open a) = a
unbound (Closed a) = a

swapBound :: Bound a -> Bound a
swapBound (Open a) = Closed a
swapBound (Closed a) = Open a


findInterval :: Rational -> SF Rational a -> ((Bound Rational, Bound Rational), a)
findInterval x (SF m def) =
  case (M.lookupLE (Open x) m, M.lookupGE (Closed x) m) of
    (Just (lo, _), Just (hi, v)) -> ((lo, hi), v)
    (Nothing, Just (hi, v)) -> ((Closed 0, swapBound hi), v)
    (Just (lo, _), Nothing) -> ((swapBound lo, Closed 1), def)
    (Nothing, Nothing) -> ((Closed 0, Closed 1), def)

intervals :: SF Rational a -> [((Rational, Rational), a)]
intervals (SF m def) = go 0 $ M.toAscList $ M.mapKeysMonotonic unbound m
  where
    go lo []
      | lo <= 1 = pure ((lo, 1), def)
      | otherwise = mempty
    go lo ((hi, a) : as) = ((lo, hi), a) : go hi as

data Durated a = Durated
  { getDuration :: Rational
  , getValue :: a
  }
  deriving stock (Eq, Ord, Show)


mapDuration :: (Rational -> Rational) -> Durated a -> Durated a
mapDuration f (Durated d a) = Durated (f d) a



foldInterval :: Rhythm a -> [(Rational, Durated a)]
foldInterval Empty = mempty
foldInterval (Full a) = pure (0, Durated 1 a)
foldInterval (Interval as) =
  flip foldMap (intervals as) $ \((lo, hi), r) ->
    fmap ((+ lo) . (* (hi - lo)) *** mapDuration (* (hi - lo))) $ foldInterval r
foldInterval (Par as bs) = foldInterval as <> foldInterval bs


joinNotes :: Eq a => [(Rational, Durated a)] -> [(Rational, Durated a)]
joinNotes [] = []
joinNotes [x] = [x]
joinNotes (n1@(o1, Durated d1 a1) : n2@(o2, Durated d2 a2) : xs)
  | o1 + d1 == o2
  , a1 == a2
  = joinNotes $ (o1, Durated (d1 + d2) a1) : xs
  | otherwise
  = n1 : joinNotes (n2 : xs)

trim :: (Bound Rational, Bound Rational) -> Rhythm a -> Rhythm a
trim _ Empty = Empty
trim _ (Full a) = Full a
trim bs (Par x y) = Par (trim bs x) (trim bs y)
trim bs (Interval sf) =
  Interval $ trimSF bs sf


trimSF :: (Bound Rational, Bound Rational) -> SF Rational a -> SF Rational a
trimSF (lo, hi) sf@(SF m def) =
  flip SF (snd $ findInterval (unbound hi) sf) $ M.fromAscList $ do
    let size = unbound hi - unbound lo
    -- lol wtf
    -- completely borked
    (i, a) <- M.toList m
    -- wtf????? i is the top of an interval, so
    guard $ lo <= i
    guard $ i <= hi
    pure (fmap ((/ size) . subtract (unbound lo)) i, a)

overlay :: (a -> b -> c) -> Rhythm a -> Rhythm b -> Rhythm c
overlay _ Empty _ = Empty
overlay _ _ Empty = Empty
overlay f (Full a) b = fmap (f a) b
overlay f a (Full b) = fmap (flip f b) a
overlay f (Par a b) c = Par (overlay f a c) (overlay f b c)
overlay f a (Par b c) = Par (overlay f a b) (overlay f a c)
overlay f (Interval as) (Interval bs) =
  -- the bug here is that liftA2 sees the whole value that overlaps
  -- but the overlap here is only partial given by the intersection of the
  -- intervals
  -- so we actually want to do a recursive call on the intersecting bits
  Interval $ liftA2SF (\bounds ar br -> overlay f (trim bounds ar) (trim bounds br)) as bs

instance Applicative Rhythm where
  pure = Full
  (<*>) = ap

instance Monad Rhythm where
  return = pure
  Full a >>= f = f a
  Empty >>= _ = Empty
  Interval as >>= f = Interval $ fmap (>>= f) as
  Par as bs >>= f = Par (as >>= f) (bs >>= f)

instance (Eq a, Show a) => EqProp (Rhythm a) where
  a =-= b = property $ \ix ->
    mu a ix === mu b ix

evenly :: [Rhythm a] -> Rhythm a
evenly [] = Empty
evenly [a] = a
evenly (init &&& last -> (as, def)) = Interval $
  flip SF def $ M.fromAscList $ do
    let len = fromIntegral $ length as + 1
    (ix, a) <- zip [1..] as
    pure (Open (ix % len), a)


listShrinker :: [a] -> [[a]]
listShrinker [_] = []
listShrinker as = do
  ix <- [0 .. length as - 1]
  pure $ do
    (ix', a) <- zip [0..] as
    guard $ ix /= ix'
    pure a


instance Arbitrary a => Arbitrary (Rhythm a) where
  arbitrary = oneof
    [ pure Empty
    , fmap Full arbitrary
    , sized $ \sz -> do
        n <- arbitrary
        fmap evenly $ vectorOf n $ resize (sz `div` n) arbitrary
    ]
  shrink Empty = []
  shrink (Full a) = Empty : fmap Full (shrink a)
  shrink (Par a b) = Empty : a : b : fmap (Par a) (shrink b) ++ fmap (flip Par b) (shrink a)
  shrink (Interval (SF as b)) = Empty : b : (fmap (Interval . flip SF b . M.fromAscList) $ listShrinker (M.toList as)) ++ fmap (Interval . SF as) (shrink b) ++ do
    (ix, a) <- M.toList as
    a' <- shrink a
    pure $ Interval $ flip SF b $ M.fromList $ do
      xx@(ix', _) <- M.toList as
      case ix == ix' of
        True -> pure (ix, a')
        False -> pure xx


test :: Rhythm String
test = evenly [pure "a", pure "b", pure "c"]

test2 :: Rhythm String
test2 = evenly [pure "1", pure "2"]

type Music = Rhythm

-- test3 :: Music String
-- test3 = Interval $ replicate 6 $ pure "."

-- test4 :: Music String
-- test4 = Interval
--   [ pure "a"
--   , Interval $ fmap pure
--     [ "b"
--     , "b"
--     , "a"
--     ]
--   ]

-- test5 :: Music String
-- test5 = Interval $ fmap pure
--   [ "a."
--   , "a."
--   , "a."
--   , "b."
--   , "b."
--   , "a."
--   ]


zz = Interval (fromList [(Open (1 % 2),Interval (fromList [(Open (1 % 2),Full "")] Empty))] Empty)


main :: IO ()
main = hspec $ modifyMaxSuccess (const 100000) $ do
  prop "overlay" $ \(Fn2 (f :: Int -> Int -> Int)) z -> do
    let l = overlay f z z
        r = fmap (\x -> f x x) z
    counterexample ("lhs: " <> show l) $
      counterexample ("rhs: " <> show r) $
        l =-= r

  focus $ prop "trim" $ \(NonEmpty (as :: [Rhythm Int])) -> do
    let len = fromIntegral $ length as
        width = recip len
    n <- elements [ 0 .. round len - 1 ]
    pure $ do
      let l = trim (Closed (fromIntegral n * width), Open ((fromIntegral n + 1) * width)) (evenly as)
          r = as !! n
      counterexample ("n: " <> show n) $
        counterexample ("lhs: " <> show l) $
          counterexample ("rhs: " <> show r) $
            l =-= r

  prop "simple trim l" $ \(a :: Rhythm String) (b :: Rhythm String) ->
    trim (Closed 0, Open $ 1 % 2) (evenly [a, b]) =-= a

  prop "simple trim r" $ \(a :: Rhythm String) (b :: Rhythm String) ->
    trim (Closed 0.5, Open 1) (evenly [a, b]) =-= b

  prop "wtf case" $ \(Fn2 (f :: Int -> Int -> Int)) a1 a2 b1 b2 b3 -> do
    let l = overlay f (evenly $ fmap pure [a1, a2]) (evenly $ fmap pure [b1, b2, b3])
        r = evenly $ fmap pure
              [ f a1 b1, f a1 b1
              , f a1 b2, f a2 b2
              , f a2 b2, f a2 b3
              ]
    counterexample ("lhs: " <> show l) $
      counterexample ("rhs: " <> show r) $
        l =-= r

