module DenoMusic.Utils where

import Control.Applicative
import Data.Bifunctor
import Data.Foldable
import Data.Function.Step.Discrete.Open (SF (..))
import Data.Functor ((<&>))
import Data.Group
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Data.Maybe (fromJust, maybeToList)
import DenoMusic.Harmony
import DenoMusic.Types
import Witherable

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

contour :: Group a => a -> (Rational -> Rational) -> Music v () -> Music v a
contour a f m = do
  let d = duration m
      sampleTime (Interval mlo mhi) =
        case (mlo, mhi) of
          (Just lo, Just hi) -> Just $ lo + (hi - lo) / 2
          _ -> mlo <|> mhi
  mapMaybe (fmap (pow a . round @_ @Int) . fmap (f $) . fmap (/ d) . sampleTime) . fmap fst $ intervals m

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

alternating :: Music () () -> Music () ()
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

-- | Push a key change over a set of voices. Unlike 'chordChange', all voices
-- will respond to the change. However, this combinator *cannot* affect the
-- chord dimension, which would lead to odd harmonies.
keyChange :: Finite v => Rational -> T ns -> Music v (T (chord ': ns))
keyChange d t = everyone $ note d $ sink t

-- | Each voice is full of its label.
voices :: Finite v => Music v v
voices = fromVoices pure
