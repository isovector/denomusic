module DenoMusic.Utils where

import Data.Function.Step.Discrete.Open
import Data.Map qualified as M
import Data.Monoid
import DenoMusic.Types


-- | Attach a label to an anonymous voice.
voice :: Eq v => v -> Music () a -> Music v a
voice v (Music m) = Music $
  \case
    ((== v) -> True) -> m ()
    _ -> Empty


-- | Build a 'Music' by attaching a label to an anonymous voice.
voiceV :: Eq v => v -> Voice a -> Music v a
voiceV v = voice v . Music . const

-- | Copy an anonymous voice to all voices.
everyone :: Music () a -> Music v a
everyone (Music m) = Music $ const $ m ()


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
