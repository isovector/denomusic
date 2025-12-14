{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Notation2 where

import Data.Function
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.IntervalMap.FingerTree qualified as IM
import Data.List (sortOn, groupBy)
import Data.Tree.DUAL
import Lilypond qualified as L
import MadMusic (fromReg)
import Music
import Notation (finalizeLily, header)
import System.Cmd (rawSystem)
import Text.PrettyPrint.HughesPJClass (pPrint)


toVoices :: Music -> [IntervalMap Rational ([L.PostEvent], Reg PitchClass)]
toVoices
  = fmap toIM
  . groupBy (on (==) $ e_voice . fst)
  . sortOn (e_voice . fst)
  . fmap (\(t, e) -> (e, export e t))
  . flatten
  . unMusic


toIM :: [(Envelope, Reg a)] -> IntervalMap Rational ([x], Reg a)
toIM = foldMap $ \(e, a) ->
  let o = e_offset e
      s = e_duration e
      lo = min o (o + s)
      hi = max o (o + s)
    in IM.singleton (Interval lo hi) (mempty, a)


toLilypond :: Music -> String
toLilypond = finalizeLily . fmap (fmap (fmap fromReg)) . toVoices


toPdf :: Music -> IO ()
toPdf m = do
  let lp = read @String $ show $ pPrint $ toLilypond m
  writeFile "/tmp/out.lily" $ header <> lp
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()
