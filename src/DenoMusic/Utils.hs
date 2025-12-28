module DenoMusic.Utils where

import Data.Functor ((<&>))
import Data.Function.Step.Discrete.Open
import Data.Map qualified as M
import Data.Monoid
import DenoMusic.Types


-- | Attach a label to an anonymous voice.
voice :: Eq v => v -> Music () a -> Music v a
voice v m@(Music sf) = Music $
  \case
    ((== v) -> True) -> sf ()
    _ -> restV $ duration m


-- | Stretch a piece of music along the time axis. Eg, @'stretch' 2 m@ will
-- make @m@ take twice as long.
stretch
  :: Rational
  -- ^ Stretch time by multiplying it against this.
  -> Music v a
  -> Music v a
stretch r (Music m) = Music $ m <&> \case
  Voice d (SF sf e) -> Voice (fmap (* r) d) $ SF (M.mapKeys (* r) sf) e
  Drone a -> Drone a
  Empty -> Empty


duration :: (Enum v, Bounded v) => Music v a -> Rational
duration (Music m) = maximum $ 0 : do
  v <- enumFromTo minBound maxBound
  case m v of
    Voice d _ -> pure $ getSum d
    Drone {} -> mempty
    Empty {} -> mempty


-- | Build a 'Music' by attaching a label to an anonymous voice.
voiceV :: Eq v => v -> Voice a -> Music v a
voiceV v = voice v . Music . const

-- | Copy an anonymous voice to all voices.
everyone :: Music () a -> Music v a
everyone (Music m) = Music $ const $ m ()


-- | Delay a voice by some offset.
delay :: Rational -> Music d a -> Music d a
delay d (Music m) = Music $ fmap (delayV d) m

-- | Delay a voice by some offset.
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


-- | Play the given 'Music's sequentially.
line :: (Foldable t, Semigroup a) => t (Music v a) -> Music v a
line = foldr (##) $ everyone $ rest 0


-- | A discrete single note of 'Voice'.
region
  :: Rational
  -- ^ Start time
  -> Rational
  -- ^ Stop time
  -> a
  -> Voice a
region lo hi a
  = Voice (pure $ hi - lo)
  $ SF (M.fromList [(lo, Nothing), (hi, Just a)]) Nothing


-- | A single note, starting at 0, for the given duration.
noteV :: Rational -> a -> Voice a
noteV = region 0


-- | A single note, starting at 0, for the given duration.
note :: Rational -> a -> Music () a
note d = voiceV () . noteV d


-- | A single rest, for the given duration.
restV :: Rational -> Voice a
restV d = Voice (pure d) $ pure Nothing


-- | A single rest, for the given duration. Rests may have negative duration,
-- which allows for synchronization of musical moments.
rest :: Rational -> Music () a
rest = voiceV () . restV


-- | Map over each voice.
withVoice
  :: (v -> Music () a -> Music () b)
  -> Music v a
  -> Music v b
withVoice f (Music m) =
  Music $ \v -> getVoices (f v $ Music $ const $ m v) ()


-- | Build a 'Music' from a function of voice labels to anonymous voice.
fromVoices :: (v -> Music () a) -> Music v a
fromVoices f = Music $ \v -> getVoices (f v) ()

-- | Split a voice at a given time
separateV :: Rational -> Voice a -> (Voice a, Voice a)
separateV _ Empty = (Empty, Empty)
separateV _ (Drone a) = (Drone a, Drone a)
separateV t (Voice d (SF sf e)) =
  let (l, r) = M.split t sf
   in ( Voice (Sum t)
          $ SF (maybe id (const id) (M.lookupMin r) l) Nothing
      , Voice (fmap (subtract t) d)
          $ SF
            (M.insert 0 Nothing
              $ M.mapKeys (subtract t)
              $ M.filterWithKey (\z _ -> z > 0) r
            ) e
      )

-- | Split a piece of a music at a given time.
separate :: Rational -> Music v a -> (Music v a, Music v a)
separate t (Music m) = do
  let vs = fmap (separateV t) m
  (Music $ fst . vs, Music $ snd . vs)


-- | Split a piece of music into @t@-sized chunks.
split :: (Bounded v, Enum v) => Rational -> Music v a -> [Music v a]
split t m =
  case t < duration m of
    True ->
      let (l, r) = separate t m
       in l : split t r
    False -> pure m
