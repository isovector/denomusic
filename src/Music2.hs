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

import Data.Set (Set)
import Data.Foldable
import Control.Monad
import Data.Function.Step.Discrete.Open
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map qualified as M
import Data.Monoid
import Data.Profunctor
import GHC.Generics
import Music.Notation (finalizeLily, header, footer)
import Music.Types (PitchClass(..), Reg(..), T(..))
import System.Cmd (rawSystem)
import Text.PrettyPrint.HughesPJClass (pPrint)

newtype Music v a = Music
  { getVoices :: v -> Voice a
  }
  deriving stock (Functor, Generic)
  deriving (Semigroup, Monoid) via (v -> Voice a)

instance Profunctor Music where
  lmap f (Music m) = Music $ m . f
  rmap = fmap

voice :: (Eq v, Monoid a) => v -> Music () a -> Music v a
voice v (Music m) = Music $
  \case
    ((== v) -> True) -> m ()
    _ -> mempty

voiceV :: (Eq v, Monoid a) => v -> Voice a -> Music v a
voiceV v va = Music $
  \case
    ((== v) -> True) -> va
    _ -> mempty

everyone :: Voice a -> Music v a
everyone = Music . pure


data Voice a
  = Voice (Sum Rational) (SF Rational a)
  | Drone a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Voice a

instance Applicative Voice where
  pure = Drone
  liftA2 f (Drone a) (Drone b) = Drone $ f a b
  liftA2 f (Voice d1 a) (Drone b) = Voice d1 $ fmap (flip f b) a
  liftA2 f (Drone a) (Voice d2 b) = Voice d2 $ fmap (f a) b
  liftA2 f (Voice d1 a) (Voice d2 b) =
    Voice (d1 <> d2) $ liftA2 f a b


delay :: Rational -> Voice a -> Voice a
delay o (Voice d (SF m e)) = Voice d $ SF (M.mapKeys (+ o) m) e
delay _ (Drone a) = Drone a


-- | Tile product (eg "play this before that")
(##) :: Semigroup a => Music v a -> Music v a -> Music v a
Music m1 ## Music m2 = Music $ liftA2 (##.) m1 m2
infixr 6 ##

-- | Tile product (eg "play this before that")
(##.) :: Semigroup a => Voice a -> Voice a -> Voice a
(##.) v1@(Voice d _) = liftA2 (<>) v1 . delay (getSum d)
(##.) (Drone a) = fmap (a <>)
infixr 6 ##.


lineV :: (Foldable t, Monoid a) => t (Voice a) -> Voice a
lineV = foldr (##.) $ restV 0

line :: (Foldable t, Monoid a) => t (Music v a) -> Music v a
line = foldr (##) $ everyone $ restV 0


region
  :: Monoid a
  => Rational
  -- ^ Start time
  -> Rational
  -- ^ Stop time
  -> a
  -> Voice a
region lo hi a = Voice (pure $ hi - lo) $ SF (M.fromList [(lo, mempty), (hi, a)]) mempty


noteV :: Monoid a => Rational -> a -> Voice a
noteV = region 0

note :: Monoid a => Rational -> a -> Music () a
note d = voiceV () . noteV d


restV :: Monoid a => Rational -> Voice a
restV d = Voice (pure d) mempty

rest :: Monoid a => Rational -> Music () a
rest = voiceV () . restV

flatten :: Voice a -> [(Interval Rational, a)]
flatten (Voice _ (SF m _)) = do
  let m' = filter ((>= 0) . fst) $ M.toList m
  ((lo, _), (hi, ma)) <- zip ((0, error "bad") : m') m'
  guard $ lo /= hi
  pure (Interval lo hi, ma)
flatten Drone {} = mempty


sample :: Rational -> Voice a -> a
sample t (Voice _ sf) = sf ! t
sample _ (Drone a) = a

--------------------------------------------------------------------------------

toVoices
  :: (Enum v, Bounded v, Foldable t)
  => Music v (t c)
  -> [[(Interval Rational, Either a ([b], c))]]
toVoices (Music m) = do
  v <- enumFromTo minBound maxBound
  pure $ do
    (int, tc) <- flatten $ m v
    c <- toList tc
    pure (int, Right ([], c))

fromVoices :: (v -> Music () a) -> Music v a
fromVoices f = Music $ \v -> getVoices (f v) ()


toLilypond :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> String
toLilypond = finalizeLily . toVoices


toPdf :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> IO ()
toPdf m = do
  let lp = read @String $ show $ pPrint $ toLilypond m
  writeFile "/tmp/out.lily" $ header <> lp <> footer
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()

mapWithVoice :: (v -> a -> b) -> Music v a -> Music v b
mapWithVoice f (Music m) = Music $ \v -> fmap (f v) $ m v


