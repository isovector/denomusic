{-# OPTIONS_GHC -fno-warn-deprecations  #-}

module Music2
  ( module Music2
  , Set
  , Profunctor(..)
  , PitchClass(..)
  , Reg(..)
  , T(..)
  , Interval(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Function.Step.Discrete.Open
import Data.Functor.Compose
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Data.Profunctor
import Data.Set (Set)
import GHC.Generics
import Music.Notation (finalizeLily, header, footer)
import Music.Types (PitchClass(..), Reg(..), T(..))
import System.Cmd (rawSystem)
import Text.PrettyPrint.HughesPJClass (pPrint)
import Test.QuickCheck (Arbitrary(..), oneof, resize, sized)
import Test.QuickCheck.Checkers (EqProp(..))

newtype Music v a = Music
  { getVoices :: v -> Voice a
  }
  deriving stock (Functor, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving Applicative via (Compose ((->) v) Voice)

instance (Enum v, Bounded v, Ord v, Show v, Show a) => Show (Music v a) where
  show = show . toVoices

instance Alternative (Music v) where
  empty = Music $ const empty
  Music m1 <|> Music m2 = Music $ \v -> m1 v <|> m2 v

instance (Enum v, Bounded v) => Foldable (Music v) where
  foldMap f (Music m) =
    foldMap (\v -> foldMap f $ m v) $ enumFromTo minBound maxBound

toVoices :: (Enum v, Bounded v, Ord v) => Music v a -> M.Map v (Voice a)
toVoices (Music m) = M.fromList $ do
  v <- enumFromTo minBound maxBound
  pure (v, m v)

unsafeFromVoices
  :: Ord v
  => M.Map v (Voice a)
  -> Music v a
unsafeFromVoices m = Music $ \v -> m M.! v

instance (Enum v, Bounded v, Ord v) => Traversable (Music v) where
  traverse f m
    = fmap unsafeFromVoices
    $ traverse (traverse f)
    $ toVoices m

instance Profunctor Music where
  lmap f (Music m) = Music $ m . f
  rmap = fmap

voice :: Eq v => v -> Music () a -> Music v a
voice v (Music m) = Music $
  \case
    ((== v) -> True) -> m ()
    _ -> Empty

voiceV :: Eq v => v -> Voice a -> Music v a
voiceV v = voice v . Music . const

everyone :: Music () a -> Music v a
everyone (Music m) = Music $ const $ m ()


data Voice a
  = Voice (Sum Rational) (SF Rational (Maybe a))
  | Drone a
  | Empty
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance EqProp a => EqProp (Voice a) where
  v1 =-= v2 = sample v1 =-= sample v2

instance (Show v, Arbitrary v, EqProp a) => EqProp (Music v a) where
  Music m1 =-= Music m2 = m1 =-= m2

instance (Arbitrary a, Semigroup a) => Arbitrary (Voice a) where
  arbitrary =
    sized $ \n -> oneof $
      case n <= 0 of
        True -> small
        False ->
          [ (##.) <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          , delayV <$> arbitrary <*> resize (n - 1) arbitrary
          ] <> small
      where
        small =
          [ noteV <$> arbitrary <*> arbitrary
          , restV <$> arbitrary
          , pure <$> arbitrary
          ]
  shrink (Drone a) = mconcat
    [ Drone <$> shrink a
    , pure Empty
    ]
  shrink Empty = []
  shrink (Voice d a) = mconcat
    [ Voice <$> shrink d <*> pure a
    , Voice <$> pure d <*> shrink a
    , pure Empty
    ]

instance (Ord v, Arbitrary v, Arbitrary a, Semigroup a) => Arbitrary (Music v a) where
  arbitrary =
    sized $ \n -> oneof $
      case n <= 0 of
        True -> small
        False ->
          [ (##) <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          , (<>) <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          ] <> small
      where
        small =
          [ voiceV <$> arbitrary <*> arbitrary
          , pure <$> arbitrary
          , pure empty
          ]

instance Semigroup a => Semigroup (Voice a) where
  Empty <> v = v
  v <> Empty = v
  Drone a <> Drone b = Drone (a <> b)
  Drone a <> Voice d s = Voice d (fmap (Just a <>) s)
  Voice d s <> Drone b = Voice d (fmap (<> Just b) s)
  Voice d1 s1 <> Voice d2 s2 = Voice (d1 <> d2) (s1 <> s2)

instance Semigroup a => Monoid (Voice a) where
  mempty = Empty

instance Applicative Voice where
  pure = Drone
  liftA2 _ Empty _ = Empty
  liftA2 _ _ Empty = Empty
  liftA2 f (Drone a) (Drone b) = Drone $ f a b
  liftA2 f (Voice d1 a) (Drone b) = Voice d1 $ fmap (fmap $ flip f b) a
  liftA2 f (Drone a) (Voice d2 b) = Voice d2 $ fmap (fmap $ f a) b
  liftA2 f (Voice d1 a) (Voice d2 b) =
    Voice (d1 <> d2) $ liftA2 (liftA2 f) a b

instance Alternative Voice where
  empty = Empty
  Empty <|> v = v
  v <|> Empty = v
  Drone a <|> _ = Drone a
  Voice d s <|> Drone a = Voice d $ fmap (<|> Just a) s
  Voice d1 s1 <|> Voice d2 s2 = Voice (d1 <> d2) $ liftA2 (<|>) s1 s2

delayV :: Rational -> Voice a -> Voice a
delayV o (Voice d (SF m e)) = Voice d $ SF (M.mapKeys (+ o) m) e
delayV _ (Drone a) = Drone a
delayV _ Empty = Empty


-- | Tile product (eg "play this before that")
(##) :: Semigroup a => Music v a -> Music v a -> Music v a
Music m1 ## Music m2 = Music $ liftA2 (##.) m1 m2
infixr 6 ##

-- | Tile product (eg "play this before that")
(##.) :: Semigroup a => Voice a -> Voice a -> Voice a
(##.) v1@(Voice d _) = (<>) v1 . delayV (getSum d)
(##.) (Drone a) = fmap (a <>)
(##.) Empty = id
infixr 6 ##.

line :: (Foldable t, Semigroup a) => t (Music v a) -> Music v a
line = foldr (##) $ everyone $ rest 0


region
  :: Rational
  -- ^ Start time
  -> Rational
  -- ^ Stop time
  -> a
  -> Voice a
region lo hi a = Voice (pure $ hi - lo) $ SF (M.fromList [(lo, Nothing), (hi, Just a)]) Nothing


noteV :: Rational -> a -> Voice a
noteV = region 0

note :: Rational -> a -> Music () a
note d = voiceV () . noteV d


restV :: Rational -> Voice a
restV d = Voice (pure d) $ pure Nothing

rest :: Rational -> Music () a
rest = voiceV () . restV

flatten :: Voice a -> [(Interval Rational, a)]
flatten (Voice _ (SF m _)) = do
  let m' = filter ((>= 0) . fst) $ M.toList m
  ((lo, _), (hi, ma)) <- zip ((0, error "bad") : m') m'
  guard $ lo /= hi
  a <- maybeToList ma
  pure (Interval lo hi, a)
flatten Drone {} = mempty
flatten Empty = mempty


sample :: Voice a -> Rational -> Maybe a
sample (Voice _ sf) t = sf ! t
sample (Drone a) _ = Just a
sample Empty _ = Nothing

--------------------------------------------------------------------------------

toNotationVoices
  :: (Enum v, Bounded v, Foldable t)
  => Music v (t c)
  -> [[(Interval Rational, Either a ([b], c))]]
toNotationVoices (Music m) = do
  v <- enumFromTo minBound maxBound
  pure $ do
    (int, tc) <- flatten $ m v
    c <- toList tc
    pure (int, Right ([], c))

fromVoices :: (v -> Music () a) -> Music v a
fromVoices f = Music $ \v -> getVoices (f v) ()


toLilypond :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> String
toLilypond = finalizeLily . toNotationVoices


toPdf :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> IO ()
toPdf m = do
  let lp = read @String $ show $ pPrint $ toLilypond m
  writeFile "/tmp/out.lily" $ header <> lp <> footer
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()

withVoice
  :: (v -> Music () a -> Music () b)
  -> Music v a
  -> Music v b
withVoice f (Music m) =
  Music $ \v -> getVoices (f v $ Music $ const $ m v) ()

