{-# LANGUAGE OverloadedLists #-}

module Galant where

import Data.Foldable
import DenoMusic.Types
import DenoMusic.Play qualified as Play
import DenoMusic.Harmony
import Data.Set qualified as S
import Data.Set (Set)
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MM
import FRP


data Voice = Top | Middle | Bottom
  deriving stock (Eq, Ord, Show, Enum, Bounded)


romanesca :: SF m (MetaScales [s, c] a) (Event (MonoidalMap Voice (Set (T [s, c]))))
romanesca = proc _ -> do
  t <- fmap round $ localTime -< ()
  ev <- takeEvents 6 <<< every 1 () -< ()
  let top = MM.singleton Top (S.singleton $ [mod (2 - t) 7, 0])
      middle = MM.singleton Middle (S.singleton $ [mod (- t) 7, 0])
      bottom = MM.singleton Bottom $ S.singleton $ [[0, 4, 5, 2, 3, 0] !! t, 0]
  returnA -< (top <> middle <> bottom) <$ ev


song :: SF (Reg PitchClass) () ()
song = proc _ -> do
  let scale = MSCons diatonic spelledSharp
  ch <- arr (mkChord . fold) <<< hold (MM.singleton Top (S.fromList [[0,0], [1,0]])) <<< romanesca -< scale
  t <- fmap (* 4) localTime -< ()
  e <- every 0.25 0.25 -< ()
  emit -< fmap (, elim (MSCons ch scale) (Reg 4 C) [round t, 0, 0]) e
  returnA -< ()

mkChord :: Set (T [x, y]) -> MetaScale 3
mkChord = UnsafeMetaScale . S.map tHead

tHead :: T (x ': xs) -> Int
tHead (x :> _) = x

main :: IO ()
main = do
  let ns = export (0, 8) song
  Play.play ns

