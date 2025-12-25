module DenoMusic.Harmony where

import Music.Play (toStupidEuterpeaPitchClass)
import Control.Arrow
import Euterpea qualified as E
import Data.Set qualified as S
import Music2
import Music.Harmony

quadruple
  :: (Enum v, Bounded v)
  => Scale PitchClass
  -- ^ Chromatic scale
  -> (v -> Reg PitchClass)
  -- ^ Mapping from voices to chord tones
  -> Music () T
  -- ^ Global chromatic progression
  -> Music () (Scale PitchClass)
  -- ^ Global scale progression
  -> Music () T
  -- ^ Global modulation within the scale
  -> Music v T
  -- ^ Voices moving in their chord
  -> Music v (Set (Reg PitchClass))
quadruple chroma voiceMap chromProg scaleProg modu m = do
  let chord = S.fromList
            $ fmap (voiceMap $)
            $ enumFromTo minBound maxBound
  (\c sc mo (v, vt) ->
    S.map (move1 chroma (move sc (mo <> vt) chord) c . move1 sc chord (mo <> vt))
      $ S.singleton $ voiceMap v
    )
    <$> everyone chromProg
    <*> everyone scaleProg
    <*> everyone modu
    <*> attachVoice m

attachVoice :: Music v a -> Music v (v, a)
attachVoice = withVoice $ \v -> fmap (v,)

play :: (Enum v, Bounded v) => Music v (Set (Reg PitchClass)) -> IO ()
play (Music m)
  = E.playDev 2
  . fmap (first toStupidEuterpeaPitchClass)
  $ foldr1 (E.:=:)
  $ do
    v <- enumFromTo minBound maxBound
    (Interval lo hi, as) <- flatten $ m v
    pure $ foldr1 (E.:=:) $ do
      a <- S.toList as
      pure $ E.rest lo E.:+: E.note (hi - lo) (fromReg a)

    -- pure $
    --   E.rest (e_offset e) E.:+: E.note (e_duration e)
    --     (fromReg $ export e t)

