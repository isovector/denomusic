module DenoMusic.Utils where

import Data.Profunctor
import Data.Map qualified as M
import Data.Map (Map)
import Data.Group
import Control.Applicative
import Data.Maybe (fromJust)
import Control.Arrow
import Data.Functor ((<&>))
import Data.Function.Step.Discrete.Open
import DenoMusic.Types
import Witherable


-- | Attach a label to an anonymous voice.
voice :: Eq v => v -> Music () a -> Music v a
voice v (Music d sf) = Music d $
  \case
    ((== v) -> True) -> sf ()
    _ -> Voice $ SF mempty Nothing


-- | Stretch a piece of music along the time axis. Eg, @'stretch' 2 m@ will
-- make @m@ take twice as long.
stretch
  :: Rational
  -- ^ Stretch time by multiplying it against this.
  -> Music v a
  -> Music v a
stretch r (Music d m) = Music (d * r) $ m <&> \case
  Voice (SF sf e) -> Voice $ SF (M.mapKeys (* r) sf) e


-- | Copy an anonymous voice to all voices.
everyone :: Music () a -> Music v a
everyone (Music d m) = Music d $ const $ m ()


-- | Delay a voice by some offset.
delay :: Rational -> Music d a -> Music d a
delay o (Music d m) = Music o $ fmap (delayV d) m

-- | Delay a voice by some offset.
delayV :: Rational -> Voice a -> Voice a
delayV o (Voice (SF m e)) = Voice $ SF (M.mapKeys (+ o) m) e


-- | Tile product (eg "play this before that")
(##) :: Semigroup a => Music v a -> Music v a -> Music v a
Music d1 m1 ## Music d2 m2 = Music (d1 + d2) $ liftA2 (<>) m1 $ fmap (delayV d1) m2
infixr 6 ##

tile :: Semigroup a => Rational -> Voice a -> Voice a -> Voice a
tile d v1 v2 = v1 <> delayV d v2


-- | Play the given 'Music's sequentially.
line :: Semigroup a => [Music v a] -> Music v a
line = foldr (##) $ everyone $ rest 0


-- | A single note, starting at 0, for the given duration.
note :: Rational -> a -> Music () a
note d a = Music d $ const $ Voice $ SF (M.fromList
  [ (0, Nothing)
  , (d, Just a)
  ]) Nothing


-- | A single rest, for the given duration. Rests may have negative duration,
-- which allows for synchronization of musical moments.
rest :: Rational -> Music () a
rest d = Music d $ const $ Voice $ SF mempty Nothing


-- | Map over each voice.
withVoice
  :: (Enum v, Bounded v, Ord v)
  => (v -> Music () a -> Music () b)
  -> Music v a
  -> Music v b
withVoice f m = fromVoices $ \v -> f v $ lmap (const v) m


enumerate :: (Enum a, Bounded a, Ord a) => (a -> b) -> Map a b
enumerate f = M.fromList $ do
  a <- enumFromTo minBound maxBound
  pure (a, f a)

-- | Build a 'Music' from a function of voice labels to anonymous voice.
fromVoices :: (Enum v, Bounded v, Ord v) => (v -> Music () a) -> Music v a
fromVoices f =
  let vs = enumerate f
   in Music (maximum $ fmap duration vs) $ \v -> getVoices (vs M.! v) ()

-- | Split a voice at a given time
separateV :: Rational -> Voice a -> (Voice a, Voice a)
separateV t (Voice (SF sf e)) =
  let (l, r) = M.split t sf
   in ( Voice $ SF (maybe id (const id) (M.lookupMin r) l) Nothing
      , Voice $ SF
            (M.insert 0 Nothing
              $ M.mapKeys (subtract t)
              $ M.filterWithKey (\z _ -> z > 0) r
            ) e
      )

-- | Split a piece of a music at a given time.
separate :: Rational -> Music v a -> (Music v a, Music v a)
separate t (Music d m) = do
  let vs = fmap (separateV t) m
  (Music t $ fst . vs, Music (d - t) $ snd . vs)


-- | Split a piece of music into @t@-sized chunks.
split :: (Bounded v, Enum v) => Rational -> Music v a -> [Music v a]
split t m =
  case t < duration m of
    True ->
      let (l, r) = separate t m
       in l : split t r
    False -> pure m


intervalsV :: Voice a -> Voice (Interval (Maybe Rational), a)
intervalsV (Voice (SF ms e)) = do
  let l = fmap (first Just) $ M.toList ms
      ms' = M.fromList $ do
              ((lo, _), (hi, a)) <- zip ((Nothing, error "impossible interalsV") : l) l
              pure (fromJust hi, fmap (Interval lo hi, ) a)
      top = M.lookupMax ms
  Voice $ SF ms' $ fmap (Interval (fmap fst top) Nothing, ) e

intervals :: Music v a -> Music v (Interval (Maybe Rational), a)
intervals (Music d v) = Music d $ fmap intervalsV v

contour :: Group a => a -> (Rational -> Rational) -> Music v () -> Music v a
contour a f m = do
  let d = duration m
      sampleTime (Interval mlo mhi) =
        case (mlo, mhi) of
          (Just lo, Just hi) -> Just $ lo + (hi - lo) / 2
          _ -> mlo <|> mhi
  mapMaybe (fmap (pow a . round @_ @Int) . fmap (f $) . fmap (/ d) . sampleTime) . fmap fst $ intervals m

