module FRP.Extra where

import FRP


partitionBeats :: Priority -> SF (Event Beat) (Event Beat, Event Beat)
partitionBeats p = proc b -> do
  strong <- filterE ((<= p) . stress) -< b
  weak   <- filterE ((> p) . stress)  -< b
  returnA -< (strong, weak)


setDuration :: Time -> Beat -> Beat
setDuration d (Beat _ p) = Beat d p

