module DenoMusic.Utils where

import Data.Bifunctor
import Data.Foldable
import Data.Function.Step.Discrete.Open (SF (..))
import Data.Function.Step.Discrete.Open qualified as SF
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Data.Maybe (fromJust, maybeToList)
import DenoMusic.Harmony
import DenoMusic.Types

-- | Attach a label to an anonymous voice.
voice :: Ord v => v -> Music () a -> Music v a
voice v = mapVoices $ const v

-- | Stretch a piece of music along the time axis. Eg, @'stretch' 2 m@ will
-- make @m@ take twice as long.
stretch
  :: Rational
  -- ^ Stretch time by multiplying it against this.
  -> Music v a
  -> Music v a
stretch r (Music d m) =
  Music (d * r) $
    m <&> \case
      Voice (SF sf e) -> Voice $ SF (M.mapKeys (* r) sf) e

-- | Copy an anonymous voice to all voices.
everyone :: (Enum v, Bounded v, Ord v) => Music () a -> Music v a
everyone (Music d m) = Music d $ MM.fromList $ do
  v <- enumFromTo minBound maxBound
  pure (v, m MM.! ())

-- | Delay a voice by some offset.
delay :: Rational -> Music d a -> Music d a
delay o (Music d m) = Music d $ fmap (delayV o) m

-- | Delay a voice by some offset.
delayV :: Rational -> Voice a -> Voice a
delayV o (Voice (SF m e)) = Voice $ SF (M.mapKeys (+ o) m) e

-- | Tile product (eg "play this before that")
(##) :: (Ord v, Semigroup a) => Music v a -> Music v a -> Music v a
Music d1 m1 ## Music d2 m2 = Music (d1 + d2) $ MM.unionWith ((<>)) m1 $ fmap (delayV d1) m2

infixr 6 ##

tile :: Semigroup a => Rational -> Voice a -> Voice a -> Voice a
tile d v1 v2 = v1 <> delayV d v2

-- | Play the given 'Music's sequentially.
line :: (Enum v, Bounded v, Ord v, Semigroup a) => [Music v a] -> Music v a
line = foldr (##) $ everyone $ rest 0

-- | A single note, starting at 0, for the given duration.
note :: Rational -> a -> Music () a
note d a =
  Music d $
    MM.singleton () $
      Voice $
        SF
          ( M.fromList
              [ (0, Nothing)
              , (d, Just a)
              ]
          )
          Nothing

-- | A single rest, for the given duration. Rests may have negative duration,
-- which allows for synchronization of musical moments.
rest :: Rational -> Music () a
rest d = Music d $ MM.singleton () $ Voice $ SF mempty Nothing

-- | Map over each voice.
withVoice
  :: (Enum v, Bounded v, Ord v, Semigroup a)
  => (v -> Music () a -> Music () b)
  -> Music v a
  -> Music v b
withVoice f (Music d sf) = fromVoices $ \v -> f v $ Music d $ MM.singleton () $ fold $ MM.lookup v sf

enumerate :: (Enum a, Bounded a, Ord a) => (a -> b) -> Map a b
enumerate f = M.fromList $ do
  a <- enumFromTo minBound maxBound
  pure (a, f a)

-- | Build a 'Music' from a function of voice labels to anonymous voice.
fromVoices :: (Enum v, Bounded v, Ord v) => (v -> Music () a) -> Music v a
fromVoices f =
  let vs = enumerate f
   in Music (maximum $ fmap duration vs) $ MM.fromList $ do
        v <- enumFromTo minBound maxBound
        z <- maybeToList $ MM.lookup () $ getVoices (vs M.! v)
        pure (v, z)

-- | Split a voice at a given time
separateV :: Rational -> Voice a -> (Voice a, Voice a)
separateV t (Voice (SF sf e)) =
  let (l, r) = M.split (t - 0.000001) sf
   in ( Voice $ SF (maybe id (\(_, a) -> M.insert t a) (M.lookupMin r) l) Nothing
      , Voice $
          SF
            ( M.insert 0 Nothing $
                M.filterWithKey (\z _ -> z > 0) $
                  M.mapKeys (subtract t) r
            )
            e
      )

-- | Split a piece of a music at a given time.
separate :: Rational -> Music v a -> (Music v a, Music v a)
separate t (Music d m) = do
  let vs = fmap (separateV t) m
  (Music t $ fmap fst vs, Music (d - t) $ fmap snd vs)

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
        pure (fromJust hi, fmap (Interval lo hi,) a)
      top = M.lookupMax ms
  Voice $ SF ms' $ fmap (Interval (fmap fst top) Nothing,) e

intervals :: Music v a -> Music v (Interval (Maybe Rational), a)
intervals (Music d v) = Music d $ fmap intervalsV v

type Finite a = (Enum a, Bounded a, Ord a)

step :: [(Rational, Maybe a)] -> Maybe a -> Music () a
step ts a =
  Music (maximum $ 0 : fmap fst ts) $
    MM.singleton () $
      Voice $
        SF (M.fromList $ (0, Nothing) : ts) a

-- | Apply a function to the last @t@ time of a music.
varyEnd :: Finite v => Rational -> (a -> a) -> Music v a -> Music v a
varyEnd t f m =
  everyone (step [(duration m - t, Just id), (duration m, Just f)] Nothing) <*> m

-- | Repeat the music for a given amount of time. If the time is
-- not a multiple of the duration, trim the last time around.
repeatFor :: (Finite v, Semigroup a) => Rational -> Music v a -> Music v a
repeatFor d m =
  fst $
    separate d $
      line $
        replicate (ceiling $ d / duration m) m

-- | Move the first half of the given music to the end.
rotate :: (Finite v, Semigroup a) => Rational -> Music v a -> Music v a
rotate t m =
  let (x, y) = separate t m
   in y ## x

alternating :: Semigroup a => Music () a -> Music () a
alternating m =
  line $
    [ m
    , rest $ duration m
    ]

-- | Stamp a rhythm on top of a piece of music.
stamp :: Finite v => Music () () -> Music v a -> Music v a
stamp f m = everyone f *> m

-- | The "purpose" of a voice. Different voices follow harmonic changes
-- differently.
data VoicePurpose
  = MelodicVoice
  | RootVoice
  | HarmonicVoice
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance HasPurpose VoicePurpose where
  purpose = id

class HasPurpose v where
  purpose :: v -> VoicePurpose

-- | Push a chord change over a set of voices. We require 'HasPurpose' on @v@,
-- because different sorts of voices should respond differently to a chord
-- change:
--
-- * harmonic voices track the chord change
-- * root voices track the root motion, but not the chord inversions
-- * melodic voices are unaffected
chordChange
  :: (Monoid (T ns), HasPurpose v, Enum v, Bounded v, Ord v)
  => Rational
  -> T (chord ': ns)
  -> Music v (T (chord ': ns))
chordChange d t'@(_ :> zs) = fromVoices $ \v ->
  note d $ case purpose v of
    MelodicVoice -> mempty
    RootVoice -> (0 :> zs)
    HarmonicVoice -> t'


rootMotion
  :: (Monoid (T ns), HasPurpose v, Enum v, Bounded v, Ord v)
  => Rational
  -> T ns
  -> Music v (T ns)
rootMotion d t' = fromVoices $ \v ->
  note d $ case purpose v of
    RootVoice -> t'
    MelodicVoice -> mempty
    HarmonicVoice -> mempty

-- | Push a key change over a set of voices. Unlike 'chordChange', all voices
-- will respond to the change. However, this combinator *cannot* affect the
-- chord dimension, which would lead to odd harmonies.
keyChange :: Finite v => Rational -> T ns -> Music v (T (chord ': ns))
keyChange d t = everyone $ note d $ sink t

-- | Each voice is full of its label.
-- TODO(sandy): this should have infinite length!
voices :: Finite v => Music v v
voices = fromVoices pure

trimStart :: Rational -> Music v a -> Music v a
trimStart t m = snd $ separate t m

trimEnd :: Rational -> Music v a -> Music v a
trimEnd t m = fst $ separate (duration m - t) m

neighbor :: (Finite v, Semigroup a) => Rational -> (a -> a) -> Music v a -> Music v a
neighbor r f m =
  let ratio = 1 / (r + 1)
   in stretch (1 - ratio) m ## stretch ratio (fmap f m) ## m

discrete
  :: [(Rational, a)]
  -- ^ Duration
  -> Music () a
discrete ns = Music (sum $ fmap fst ns) $ MM.singleton () $ Voice $ SF.fromList ((0, Nothing) : go 0 ns) Nothing
  where
    go _ [] = []
    go t ((d, a) : as) = (t + d, Just a) : go (t + d) as

switch
  :: Monoid a
  => a
  -> Music () a
switch a = Music 0 $ MM.singleton () $ Voice $ SF (M.singleton 0 $ Just mempty) (Just a)

contour
  :: (Rational -> Maybe a)
  -- ^ Function to sample
  -> Rational
  -- ^ Duration
  -> Rational
  -- ^ Sample offsets
  -> Music () a
contour f d o = Music d $ MM.singleton () $ Voice $ flip SF.fromList Nothing $ (d, f d) : do
  ix <- [0, o .. d]
  pure (ix, f ix)

mergeDurations
  :: Eq a
  => Music v a
  -> Music v a
mergeDurations (Music d m) = Music d $ fmap (Voice . SF.normalise . unVoice) m

