import DenoMusic.NotationBackend
import DenoMusic.Types
import Data.Lilypond
import Data.IntervalMap.FingerTree (Interval(..))

main = do
  writeFile "/tmp/song.lily" $ finalizeLily $ makeScore
    [  [ (Interval 0 1, Right ([], Reg 4 C))
       , (Interval 3.875 4, Right ([], Reg 3 C))
       ]
    ,  [ (Interval 0.25 1, Right ([], Reg 5 C))
       ]
    ,  [ (Interval 0 0.5, Right ([], Reg 3 C))
       , (Interval 0.5 0.75, Right ([], Reg 3 D))
       , (Interval 0.75 1.5, Right ([], Reg 3 E))
       , (Interval 1.5 1.75, Right ([], Reg 4 E))
       ]
    ]
