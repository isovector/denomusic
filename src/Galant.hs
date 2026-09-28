{-# LANGUAGE OverloadedLists #-}

module Galant where

import DenoMusic.Harmony
import Data.Set qualified as S
import Data.Set (Set)
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MM
import FRP


data Voice = Top | Middle | Bottom
  deriving stock (Eq, Ord, Show, Enum, Bounded)


romanesca :: SF (MetaScales [s, c] a) (Event (MonoidalMap Voice (Set (T [s, c] Int))))
romanesca = proc _ -> do
  t <- fmap round $ localTime -< ()
  ev <- takeE 6 <<< every 1 () -< ()
  let top = MM.singleton Top (S.singleton $ [mod (2 - t) 7, 0])
      middle = MM.singleton Middle (S.singleton $ [mod (- t) 7, 0])
      bottom = MM.singleton Bottom $ S.singleton $ [[0, 4, 5, 2, 3, 0] !! t, 0]
  returnA -< (top <> middle <> bottom) <$ ev

