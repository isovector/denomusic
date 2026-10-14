module Ex.AutomaticVoiceLeading where

import Control.Lens
import DenoMusic.Play
import FRP
import Music.Extra
import Music.Harmony
import Music.TimeSig
import Music.Types


harmony :: SF x (Event (Deg 7))
harmony = discrete
  [ (0, 0)
  , (1, 3)
  , (2, 4)
  , (3, 0)
  ]


motive :: [Note 3]
motive = drop 1 $ scanl (+) 0
  [ 2
  , 2
  , 0 //- 1
  , 0 //+ 1
  , -1
  , -1
  , 1
  , -3

  , 4
  , -1
  , 1
  , 1
  , 0
  , -2
  , -2
  ]


song :: SF () (Event (Timed (Note 7)))
song = proc _ -> do
  vls <- msumE <<< leading' @3 <<< harmony -< ()
  let sc = apply vls triad
  bs <- beatsOf time4'4 -< ()
  bs' <- replace (0 : cycle motive) -< bs

  returnA -< fmap (\(bb, n) -> Timed (view duration bb) $ applyScale sc n) bs'


main :: IO ()
main = play $ export (0, 4) song

