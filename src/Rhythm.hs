{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Rhythm (
  -- * Core Type
  Rhythm (..),
  mu,

  -- * Constructing Sequential Rhythms
  weightedTuplet,
  tuplet,
  im,
  rtimes,

  -- * Constructing Parallel Rhythms
  asum,
  chord,

  -- * Constructing Applicative Rhythms
  overlay,

  -- * Intervals
  Interval (..),
  getDuration,
  getOffset,
  annotate,
  intervals,

  -- * Reexpots
  SF (..),
  Bound (..),

  -- * Tests
  spec,
) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad (ap, guard)
import Control.Monad.State
import Data.Foldable (toList)
import Data.Function.Step (Bound (..), SF (..))
import Data.List (unsnoc)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Traversable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers

-- | An algebraically compositional representation of music. The core idea of
-- 'Rhythm' is to give up on any absolute notion of time, and instead treat all
-- music as normalized over the interval @[0,1)@. When we want to actually
-- /play/ the music, we can multiply this interval out to the desired number of
-- measures.
--
-- The combinator 'tuplet' fills the interval with its elements, ensuring that
-- each element gets an equal division of time. Thus, we can make a triplet as
--
-- @'tuplet' [a, b, c]@
--
-- . 'tuplet' is the fundamental means of constructing rhythms. Because it can be
-- nested, it allows us to subdivide beats, eg:
--
-- @'tuplet' [quarter, 'tuplet' [ eighth, eighth ],  quarter, quarter]@
--
-- constructs an interval which is broken up into four spans, the second of
-- which is further subdivided in half. Thus, interpreted as a single measure,
-- this represents a quarter note, two eighth notes, and then two more quarter
-- notes.
--
-- The advantages of representing music this way is that it gives us a very
-- meaningful set of algebraic primitives. Namely, we have two meaningful
-- 'Applicative's, one 'Monad', and an 'Alternative'. It is these instances
-- which give us most of the interesting means of constructing music.
--
-- The 'Applicative' and 'Monad' instances are both "substitute-y", meaning
-- they will replace all "notes" with a sub 'Rhythm'. We can use this, for
-- example, to generate broken chords:
--
-- @
--   do
--     -- Fill the interval with four evenly spaced C triads.
--     chord <- 'tuplet' $ replicate 4 $ pure [(C, 3), (E, 3), (G, 3)]
--     -- Replace each triad with a its first two notes, and then everything but
--        its first two notes.
--     'tuplet' [ take 2 chord, drop 2 chord ]
-- ==
--   'tuplet' $ replicate 4 $ 'tuplet'
--     [ [(C, 3), (E, 3)]
--     , [(G, 3)]
--     ]
-- @
--
-- We also have another applicative, although due to Haskell's typesystem it
-- can't be an instance of 'Applicative'. This operation goes by the name
-- 'overlay', which instead applies a function pointwise across two 'Rhythm's.
-- We can use this, eg, to "stamp" a rhythmic element on top of chord changes.
--
-- @
-- 'overlay' 'const' pitches rhythm
-- @
--
-- which will use the pitches from @pitches@ but ensure that the rhythm from
-- @rhythm@ is also present. Of course, we need not use 'const' here:
--
-- @
--   'overlay' f ('tuplet' [a, b, c]) ('tuplet' [x, y])
-- ==
--   'tuplet'
--      [ 'overlay' f a x
--      , 'overlay' f a x
--      , 'overlay' f b x
--      , 'overlay' f b y
--      , 'overlay' f c y
--      , 'overlay' f c y
--      ]
-- @
--
-- The 'Alternative' instance performs two 'Rhythm's in parallel. For example,
--
-- @
-- 'tuplet' (replicate 3 a) <|> 'tuplet' (replicate 4 b)
-- @
--
-- is a threes-over-fours rhythm. Correspondingly, 'empty' is the absence of
-- a rhythm, which means it can be used in 'overlay' to selectively mute
-- sections of a 'Rhythm' after the fact.
data Rhythm a
  = Empty
  | Full a
  | -- | A sequential rhythm, dividing up the interval @[0,1)@.
    Seq (SF Rational (Rhythm a))
  | Par (Rhythm a) (Rhythm a)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Alternative Rhythm where
  Empty <|> a = a
  a <|> Empty = a
  a <|> b = Par a b
  empty = Empty

-- | An 'Interval' is upper and lower bounds.
data Interval a = Interval
  { i_lo :: Bound a
  , i_hi :: Bound a
  }
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)

getDuration :: Num a => Interval a -> a
getDuration (Interval lo hi) = unbound hi - unbound lo

getOffset :: Interval a -> a
getOffset = unbound . i_lo

instance Show a => Show (Interval a) where
  show (Interval lo hi) =
    mconcat
      [ case lo of
          Open {} -> "("
          Closed {} -> "["
      , show $ unbound lo
      , ","
      , show $ unbound hi
      , case hi of
          Open {} -> ")"
          Closed {} -> "]"
      ]

-- | Shorthand for @'tuplet' . fmap pure@.
im :: [a] -> Rhythm a
im = tuplet . fmap pure

-- | Shorthand for @'asum' . fmap pure@.
chord :: [a] -> Rhythm a
chord = asum . fmap pure

-- | Repeat a 'Rhythm' some number of times.
rtimes :: Int -> Rhythm a -> Rhythm a
rtimes 0 _ = Empty
rtimes 1 a = a
rtimes n a = tuplet $ replicate n a

-- | 'mu' provides a denotational semantics to 'Rhythm' by computing the set of
-- "notes"  being played simultaneously at any point in the interval.
mu :: Ord a => Rhythm a -> Rational -> Set a
mu _ n | n > 1 = mempty
mu _ n | n < 0 = mempty
mu Empty _ = mempty
mu (Full a) _ = S.singleton a
mu (Seq as) n =
  let (Interval leftb (unbound -> right), m) = findInterval n as
      left = unbound leftb
      width = right - left
   in case (width, leftb) of
        -- (0, Closed 1) -> mu m 1
        -- (0, _) -> error "how did you do this"
        _ -> mu m $ (n - left) / width
mu (Par a b) n = mu a n <> mu b n

unbound :: Bound a -> a
unbound (Open a) = a
unbound (Closed a) = a

swapBound :: Bound a -> Bound a
swapBound (Open a) = Closed a
swapBound (Closed a) = Open a

findInterval :: Rational -> SF Rational a -> (Interval Rational, a)
findInterval x (SF m def) =
  case (M.lookupLE (Open x) m, M.lookupGE (Closed x) m) of
    (Just (lo, _), Just (hi, v)) -> (Interval (swapBound lo) hi, v)
    (Nothing, Just (hi, v)) -> (Interval (Closed 0) hi, v)
    (Just (lo, _), Nothing) -> (Interval (swapBound lo) (Open 1), def)
    (Nothing, Nothing) -> (Interval (Closed 0) (Open 1), def)

intervals :: Rhythm a -> [(Interval Rational, a)]
intervals = toList . annotate

sfIntervals :: SF Rational a -> [(Interval Rational, a)]
sfIntervals (SF m def) = go (Closed 0) $ M.toAscList m
 where
  go lo [] = pure (Interval lo $ Open 1, def)
  go lo ((hi, a) : as) = (Interval lo hi, a) : go (swapBound hi) as

-- | Compute the 'Interval' associated with every "note" in a 'Rhythm'. It's
-- not entirely clear to me why you might want to do this, but it's interesting
-- that you can.
annotate :: Rhythm a -> Rhythm (Interval Rational, a)
annotate (Full a) = Full (Interval (Closed 0) (Open 1), a)
annotate Empty = Empty
annotate (Par a b) = Par (annotate a) (annotate b)
annotate (Seq sf) =
  mkInterval $ do
    (b, a) <- sfIntervals sf
    pure $ (i_hi b,) $ fmap (first $ debound b) $ annotate a

overlay :: (a -> b -> c) -> Rhythm a -> Rhythm b -> Rhythm c
overlay _ Empty _ = Empty
overlay _ _ Empty = Empty
overlay f (Full a) b = fmap (f a) b
overlay f a (Full b) = fmap (flip f b) a
overlay f (Par a b) c = Par (overlay f a c) (overlay f b c)
overlay f a (Par b c) = Par (overlay f a b) (overlay f a c)
overlay f ar@(Seq as) br@(Seq bs) =
  mkInterval $ do
    bound <- overlappingSpans as bs
    let a = getSpan bound ar
        b = getSpan bound br
    pure (i_hi bound, overlay f a b)

overlappingSpans :: SF Rational a -> SF Rational b -> [Interval Rational]
overlappingSpans (SF sfa _) (SF sfb _) =
  go (Closed 0) (fmap fst (M.toList sfa) <> pure (Open 1)) (fmap fst (M.toList sfb) <> pure (Open 1))
 where
  go _ [] [] = []
  go acc (i : as) [] = (Interval acc i) : go (swapBound i) as []
  go acc [] (i : bs) = (Interval acc i) : go (swapBound i) [] bs
  go acc as@(ia : as') bs@(ib : bs') = do
    let bounds = Interval acc $ min ia ib
    case compare ia ib of
      LT -> bounds : go (swapBound ia) as' bs
      EQ -> bounds : go (swapBound ia) as' bs'
      GT -> bounds : go (swapBound ib) as bs'

getSpan :: Interval Rational -> Rhythm a -> Rhythm a
getSpan _ Empty = Empty
getSpan _ (Full a) = Full a
getSpan bs (Par x y) = Par (getSpan bs x) (getSpan bs y)
getSpan bs (Seq sf) =
  mkInterval $ do
    (b@(Interval _ hi), a) <- sfIntervals sf
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

renormalize :: Interval Rational -> Bound Rational -> Bound Rational
renormalize (Interval (unbound -> lo) hib) x =
  fmap ((/ (unbound hib - lo)) . (subtract lo)) x

rebound :: Interval Rational -> Interval Rational -> Interval Rational
rebound b (Interval lo hi) = Interval (renormalize b lo) $ renormalize b hi

denormalize :: Interval Rational -> Bound Rational -> Bound Rational
denormalize (Interval (unbound -> lo) hib) x =
  fmap ((+ lo) . (* (unbound hib - lo))) x

debound :: Interval Rational -> Interval Rational -> Interval Rational
debound b (Interval lo hi) = Interval (denormalize b lo) (denormalize b hi)

intersection :: Ord k => Interval k -> Interval k -> Maybe (Interval k)
intersection (Interval al ar) (Interval bl br) = do
  let l = max al bl
      r = min ar br
  case compare l r of
    LT -> Just $ Interval l r
    EQ -> Just $ Interval l l
    GT -> Nothing

-- | This is a "substitute-y" 'Applicative', in that it performs tree grafting.
-- You can use the 'overlay' operation instead of 'liftA2' if you're instead
-- looking for a "zippy" applicative.
instance Applicative Rhythm where
  pure = Full
  (<*>) = ap

-- | This is a "substitute-y" 'Monad', in that it performs tree grafting.
instance Monad Rhythm where
  return = pure
  Full a >>= f = f a
  Empty >>= _ = Empty
  Seq as >>= f = Seq $ fmap (>>= f) as
  Par as bs >>= f = Par (as >>= f) (bs >>= f)

instance (Ord a, Show a) => EqProp (Rhythm a) where
  a =-= b = property $ \ix ->
    mu a ix === mu b ix

-- | @'tuplet' as@ divides the time interval equally, laying out each of @as@
-- sequentially. As its name implies, we can use this for constructing literal
-- triplets (and other tuplets), but more generally this is the fundamental
-- means of laying out 'Rhythm's sequentially. For example,
--
-- @'tuplet' measures@
--
-- will lay out a song, assigning equal time to each measure (which is exactly
-- what we mean by a musical "measure.")
tuplet :: [Rhythm a] -> Rhythm a
tuplet = weightedTuplet . fmap (1,)

weightedTuplet :: [(Rational, Rhythm a)] -> Rhythm a
weightedTuplet [] = Empty
weightedTuplet [(_, a)] = a
weightedTuplet as = do
  let duration = sum $ fmap fst as
      pieces =
        flip evalState 0 $
          for as $ \(sz, a) -> do
            here <- get
            let there = here + sz / duration
            put there
            pure (Open there, a)
  mkInterval pieces

mkInterval :: [(Bound Rational, Rhythm a)] -> Rhythm a
mkInterval pieces =
  case unsnoc pieces of
    Nothing -> Empty
    Just ([], a) -> snd a
    Just (as', a) ->
      Seq $ SF (M.fromAscList as') (snd a)

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

instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = Interval <$> arbitrary <*> arbitrary

spec :: Spec
spec = modifyMaxSuccess (const 100000) $ do
  describe "intersection" $ do
    prop "commutative" $ \x y ->
      intersection @Rational x y === intersection y x

    prop "idempotent" $ \x y -> do
      let r = intersection @Rational x y
      r === (r >>= intersection x)

    it "unit test" $
      intersection (Interval (Open @Rational 0) $ Closed 0.5) (Interval (Open 0.25) $ Open 1)
        === Just (Interval (Open 0.25) $ Closed 0.5)

  describe "getSpan" $ do
    prop "identity" $ \x ->
      getSpan @Int (Interval (Closed 0) $ Open 1) x =-= x

    prop "get one" $ \x y -> do
      let lhs =
            getSpan @Int
              (Interval (Closed 0) $ Open 0.5)
              (tuplet [x, y])
      counterexample ("lhs: " <> show lhs) $
        lhs =-= x

    prop "get two" $ \x y -> do
      let evened = tuplet [x, y]
          lhs =
            getSpan @Int
              (Interval (Closed 0.5) $ Open 1)
              evened
      counterexample ("evened: " <> show evened) $
        counterexample ("lhs: " <> show lhs) $
          lhs =-= y

    prop "cover two" $ \x y -> do
      let r = tuplet @Int [x, y]
      let lhs =
            tuplet
              [ getSpan (Interval (Closed 0) $ Open 0.5) r
              , getSpan (Interval (Closed 0.5) $ Open 1) r
              ]
      counterexample ("lhs: " <> show lhs) $
        lhs =-= r

    prop "cover three" $ \x y z -> do
      let r = tuplet @Int [x, y, z]
      let lhs =
            tuplet
              [ getSpan (Interval (Closed 0) $ Open 0.5) r
              , getSpan (Interval (Closed 0.5) $ Open 1) r
              ]
      counterexample ("even: " <> show r) $
        counterexample ("lhs: " <> show lhs) $
          lhs =-= r

    it "cover three unit" $ do
      let r = tuplet [Full "x", tuplet [Full "y1", Full "y2"], Full "z"]
      let lhs =
            tuplet
              [ getSpan (Interval (Closed 0) $ Open 0.5) r
              , getSpan (Interval (Closed 0.5) $ Open 1) r
              ]
      counterexample ("tuplet: " <> show r) $
        counterexample ("lhs: " <> show lhs) $
          lhs =-= r

  describe "overlappingSpans" $ do
    prop "id" $ \x y -> do
      let Seq sf = tuplet @Int [x, y]
          lhs = fmap fst $ sfIntervals sf
      lhs === overlappingSpans sf sf

    prop "many" $ do
      n <- fmap ((+ 2) . abs) arbitrary
      pure $
        counterexample ("n: " <> show n) $ do
          let Seq sf = tuplet @Int $ replicate n Empty
              lhs = fmap fst $ sfIntervals sf
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
