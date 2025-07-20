{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Rhythm (
  -- * Core Type
  Rhythm (..),
  mu,

  -- * Constructing Sequential Rhythms
  tuplet,
  im,
  rtimes,

  -- * Constructing Parallel Rhythms
  chord,

  -- * Constructing Applicative Rhythms
  overlay,

  -- * Eliminating Rhythms
  annotate,
  foldRhythm,
  Durated (..),
  mapDuration,

  -- * Reexpots
  SF (..),
  Bound (..),

  -- * Tests
  spec,
) where

import Control.Arrow (first, (&&&), (***))
import Control.Monad (ap, guard)
import Data.Function.Step (Bound (..), SF (..))
import Data.List (unsnoc)
import Data.Map qualified as M
import Data.Maybe
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as S
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Rhythm a
  = Empty
  | Full a
  | Seq (SF Rational (Rhythm a))
  | Par (Rhythm a) (Rhythm a)
  deriving stock (Show, Functor, Foldable, Traversable)

im :: [a] -> Rhythm a
im = tuplet . fmap pure

chord :: [a] -> Rhythm a
chord [] = Empty
chord xs = foldr1 Par $ fmap pure xs

rtimes :: Int -> Rhythm a -> Rhythm a
rtimes 0 _ = Empty
rtimes 1 a = a
rtimes n a = tuplet $ replicate n a

mu :: Ord a => Rhythm a -> Rational -> Set a
mu _ n | n > 1 = mempty
mu _ n | n < 0 = mempty
mu Empty _ = mempty
mu (Full a) _ = S.singleton a
mu (Seq as) n =
  let ((leftb, unbound -> right), m) = findInterval n as
      left = unbound leftb
      width = right - left
   in case (width, leftb) of
        (0, Closed 1) -> mu m 1
        (0, _) -> error "how did you do this"
        _ -> mu m $ (n - left) / width
mu (Par a b) n = mu a n <> mu b n

unbound :: Bound a -> a
unbound (Open a) = a
unbound (Closed a) = a

swapBound :: Bound a -> Bound a
swapBound (Open a) = Closed a
swapBound (Closed a) = Open a

findInterval :: Rational -> SF Rational a -> ((Bound Rational, Bound Rational), a)
findInterval x (SF m def) =
  case (M.lookupLE (Open x) m, M.lookupGE (Closed x) m) of
    (Just (lo, _), Just (hi, v)) -> ((swapBound lo, hi), v)
    (Nothing, Just (hi, v)) -> ((Closed 0, hi), v)
    (Just (lo, _), Nothing) -> ((swapBound lo, Closed 1), def)
    (Nothing, Nothing) -> ((Closed 0, Closed 1), def)

intervals :: SF Rational a -> [((Bound Rational, Bound Rational), a)]
intervals (SF m def) = go (Closed 0) $ M.toAscList m
 where
  go lo [] = pure ((lo, Closed 1), def)
  go lo ((hi, a) : as) = ((lo, hi), a) : go (swapBound hi) as

annotate :: Rhythm a -> Rhythm ((Bound Rational, Bound Rational), a)
annotate (Full a) = Full ((Closed 0, Closed 1), a)
annotate Empty = Empty
annotate (Par a b) = Par (annotate a) (annotate b)
annotate (Seq sf) = do
  let is = intervals sf
  case unsnoc is of
    Nothing -> Empty
    Just (pieces, (end, def)) ->
      Seq $ flip SF (fmap (first $ debound end) $ annotate def) $ M.fromAscList $ do
        (b, a) <- pieces
        let a' = annotate a
        pure (snd b, fmap (first $ debound b) $ a')

data Durated a = Durated
  { getDuration :: Rational
  , getValue :: a
  }
  deriving stock (Eq, Ord, Show)

mapDuration :: (Rational -> Rational) -> Durated a -> Durated a
mapDuration f (Durated d a) = Durated (f d) a

foldRhythm :: Rhythm a -> [(Rational, Durated a)]
foldRhythm Empty = mempty
foldRhythm (Full a) = pure (0, Durated 1 a)
foldRhythm (Seq as) =
  flip foldMap (intervals as) $ \((unbound -> lo, unbound -> hi), r) ->
    fmap ((+ lo) . (* (hi - lo)) *** mapDuration (* (hi - lo))) $ foldRhythm r
foldRhythm (Par as bs) = foldRhythm as <> foldRhythm bs

overlay :: (a -> b -> c) -> Rhythm a -> Rhythm b -> Rhythm c
overlay _ Empty _ = Empty
overlay _ _ Empty = Empty
overlay f (Full a) b = fmap (f a) b
overlay f a (Full b) = fmap (flip f b) a
overlay f (Par a b) c = Par (overlay f a c) (overlay f b c)
overlay f a (Par b c) = Par (overlay f a b) (overlay f a c)
overlay f ar@(Seq as) br@(Seq bs) = do
  let spans = do
        bound <- overlappingSpans as bs
        let a = getSpan bound ar
            b = getSpan bound br
        pure (snd bound, overlay f a b)
  case unsnoc spans of
    Nothing -> Empty
    Just ([], d) -> snd d
    Just (pieces, d) ->
      Seq $ SF (M.fromAscList pieces) $ snd d

overlappingSpans :: SF Rational a -> SF Rational b -> [(Bound Rational, Bound Rational)]
overlappingSpans (SF sfa _) (SF sfb _) =
  go (Closed 0) (fmap fst (M.toList sfa) <> pure (Closed 1)) (fmap fst (M.toList sfb) <> pure (Closed 1))
 where
  go _ [] [] = []
  go acc (i : as) [] = (acc, i) : go (swapBound i) as []
  go acc [] (i : bs) = (acc, i) : go (swapBound i) [] bs
  go acc as@(ia : as') bs@(ib : bs') = do
    let bounds = (acc, min ia ib)
    case compare ia ib of
      LT -> bounds : go (swapBound ia) as' bs
      EQ -> bounds : go (swapBound ia) as' bs'
      GT -> bounds : go (swapBound ib) as bs'

getSpan :: (Bound Rational, Bound Rational) -> Rhythm a -> Rhythm a
getSpan _ Empty = Empty
getSpan _ (Full a) = Full a
getSpan bs (Par x y) = Par (getSpan bs x) (getSpan bs y)
getSpan bs (Seq sf) = do
  let spanning = do
        (b@(_, hi), a) <- intervals sf
        intersected <- maybeToList $ intersection bs b
        let hi' = renormalize bs hi
        case b == intersected of
          True -> do
            -- can keep span intact, just need to renormalize hi
            pure (hi', a)
          False -> do
            -- otherwise we need to trim the underlying span
            let intersected' = rebound b intersected
            pure (hi', getSpan intersected' a)
  case unsnoc spanning of
    Just ([], def) -> snd def
    Just (pieces, def) ->
      Seq $ SF (M.fromAscList pieces) $ snd def
    Nothing -> Empty

renormalize :: (Bound Rational, Bound Rational) -> Bound Rational -> Bound Rational
renormalize (unbound -> lo, hib) x =
  fmap ((/ (unbound hib - lo)) . (subtract lo)) x

rebound :: (Bound Rational, Bound Rational) -> (Bound Rational, Bound Rational) -> (Bound Rational, Bound Rational)
rebound b (lo, hi) = (renormalize b lo, renormalize b hi)

denormalize :: (Bound Rational, Bound Rational) -> Bound Rational -> Bound Rational
denormalize (unbound -> lo, hib) x =
  fmap ((+ lo) . (* (unbound hib - lo))) x

debound :: (Bound Rational, Bound Rational) -> (Bound Rational, Bound Rational) -> (Bound Rational, Bound Rational)
debound b (lo, hi) = (denormalize b lo, denormalize b hi)

intersection :: Ord k => (Bound k, Bound k) -> (Bound k, Bound k) -> Maybe (Bound k, Bound k)
intersection (al, ar) (bl, br) = do
  let l = max al bl
      r = min ar br
  case compare l r of
    LT -> Just (l, r)
    EQ -> Just (l, l)
    GT -> Nothing

instance Applicative Rhythm where
  pure = Full
  (<*>) = ap

instance Monad Rhythm where
  return = pure
  Full a >>= f = f a
  Empty >>= _ = Empty
  Seq as >>= f = Seq $ fmap (>>= f) as
  Par as bs >>= f = Par (as >>= f) (bs >>= f)

instance (Ord a, Show a) => EqProp (Rhythm a) where
  a =-= b = property $ \ix ->
    mu a ix === mu b ix

tuplet :: [Rhythm a] -> Rhythm a
tuplet [] = Empty
tuplet [a] = a
tuplet (init &&& last -> (as, def)) = Seq $
  flip SF def $
    M.fromAscList $ do
      let len = fromIntegral $ length as + 1
      (ix, a) <- zip [1 ..] as
      pure (Open (ix % len), a)

listShrinker :: [a] -> [[a]]
listShrinker [_] = []
listShrinker as = do
  ix <- [0 .. length as - 1]
  pure $ do
    (ix', a) <- zip [0 ..] as
    guard $ ix /= ix'
    pure a

instance Arbitrary a => Arbitrary (Rhythm a) where
  arbitrary =
    oneof
      [ pure Empty
      , fmap Full arbitrary
      , Par <$> arbitrary <*> arbitrary
      , sized $ \sz -> do
          n <- fmap ((+ 2) . abs) arbitrary
          fmap tuplet $ vectorOf n $ resize (sz `div` n) arbitrary
      ]
  shrink Empty = []
  shrink (Full a) = Empty : fmap Full (shrink a)
  shrink (Par a b) = Empty : a : b : fmap (Par a) (shrink b) ++ fmap (flip Par b) (shrink a)
  shrink (Seq (SF as b)) =
    Empty : b : (fmap (Seq . flip SF b . M.fromAscList) $ listShrinker (M.toList as)) ++ fmap (Seq . SF as) (shrink b) ++ do
      (ix, a) <- M.toList as
      a' <- shrink a
      pure $ Seq $ flip SF b $ M.fromList $ do
        xx@(ix', _) <- M.toList as
        case ix == ix' of
          True -> pure (ix, a')
          False -> pure xx

spec :: Spec
spec = modifyMaxSuccess (const 100000) $ do
  describe "intersection" $ do
    prop "commutative" $ \x y ->
      intersection @Rational x y === intersection y x

    prop "idempotent" $ \x y -> do
      let r = intersection @Rational x y
      r === (r >>= intersection x)

    it "unit test" $
      intersection (Open @Rational 0, Closed 0.5) (Open 0.25, Closed 1)
        === Just (Open 0.25, Closed 0.5)

  describe "getSpan" $ do
    prop "identity" $ \x ->
      getSpan @Int (Closed 0, Closed 1) x =-= x

    prop "get one" $ \x y -> do
      let lhs =
            getSpan @Int
              (Closed 0, Open 0.5)
              (tuplet [x, y])
      counterexample ("lhs: " <> show lhs) $
        lhs =-= x

    prop "get two" $ \x y -> do
      let evened = tuplet [x, y]
          lhs =
            getSpan @Int
              (Closed 0.5, Closed 1)
              evened
      counterexample ("evened: " <> show evened) $
        counterexample ("lhs: " <> show lhs) $
          lhs =-= y

    prop "cover two" $ \x y -> do
      let r = tuplet @Int [x, y]
      let lhs =
            tuplet
              [ getSpan (Closed 0, Open 0.5) r
              , getSpan (Closed 0.5, Closed 1) r
              ]
      counterexample ("lhs: " <> show lhs) $
        lhs =-= r

    prop "cover three" $ \x y z -> do
      let r = tuplet @Int [x, y, z]
      let lhs =
            tuplet
              [ getSpan (Closed 0, Open 0.5) r
              , getSpan (Closed 0.5, Closed 1) r
              ]
      counterexample ("even: " <> show r) $
        counterexample ("lhs: " <> show lhs) $
          lhs =-= r

    it "cover three unit" $ do
      let r = tuplet [Full "x", tuplet [Full "y1", Full "y2"], Full "z"]
      let lhs =
            tuplet
              [ getSpan (Closed 0, Open 0.5) r
              , getSpan (Closed 0.5, Closed 1) r
              ]
      counterexample ("tuplet: " <> show r) $
        counterexample ("lhs: " <> show lhs) $
          lhs =-= r

  describe "overlappingSpans" $ do
    prop "id" $ \x y -> do
      let Seq sf = tuplet @Int [x, y]
          lhs = fmap fst $ intervals sf
      lhs === overlappingSpans sf sf

    prop "many" $ do
      n <- fmap ((+ 2) . abs) arbitrary
      pure $
        counterexample ("n: " <> show n) $ do
          let Seq sf = tuplet @Int $ replicate n Empty
              lhs = fmap fst $ intervals sf
          lhs === overlappingSpans sf sf

  describe "overlay" $ do
    prop "const same" $ \(z :: Rhythm Int) -> do
      let l = overlay const z z
          r = z
      counterexample ("lhs: " <> show l) $
        counterexample ("rhs: " <> show r) $
          l =-= r

    prop "const different" $ \(x :: Rhythm Int) (y :: Rhythm Int) -> do
      let l = overlay const x y
          r = x
      ix <- arbitrary
      pure $
        counterexample ("lhs: " <> show l) $
          counterexample ("rhs: " <> show r) $
            counterexample ("at: " <> show ix) $
              case null $ mu y ix of
                True -> mu l ix === mempty
                False -> mu l ix === mu r ix

    prop "everywhere vs pure" $ \(Fn2 (f :: Int -> Int -> Int)) x y -> do
      nx <- fmap ((+ 2) . abs) arbitrary
      ny <- fmap ((+ 2) . abs) arbitrary
      let l = overlay f (tuplet $ replicate nx $ pure x) (tuplet $ replicate ny $ pure y)
          r = pure $ f x y
      pure $
        counterexample ("lhs: " <> show l) $
          counterexample ("rhs: " <> show r) $
            l =-= r

    prop "pointwise" $ \(Fn2 (f :: Int -> Int -> Int)) x y -> do
      r <- arbitrary
      pure $
        mu (overlay f x y) r === S.fromList (f <$> S.toList (mu x r) <*> S.toList (mu y r))

  prop "wtf case" $ \(Fn2 (f :: Int -> Int -> Int)) a1 a2 b1 b2 b3 -> do
    let l = overlay f (tuplet $ fmap pure [a1, a2]) (tuplet $ fmap pure [b1, b2, b3])
        r =
          tuplet $
            fmap
              pure
              [ f a1 b1
              , f a1 b1
              , f a1 b2
              , f a2 b2
              , f a2 b2
              , f a2 b3
              ]
    counterexample ("lhs: " <> show l) $
      counterexample ("rhs: " <> show r) $
        l =-= r
