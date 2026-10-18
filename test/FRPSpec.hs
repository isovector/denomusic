module FRPSpec where

import FRP
import Music.Types
import SignalSpec ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "hitDyn should time properly" $ \n1 n2 (Positive t1) (Positive t2) -> do
    let note1 = fromIntegral @Int @(Note 7) n1
        note2 = fromIntegral @Int @(Note 7) n2
    shouldBe
      ( observe $ horizontal $ hit t1 note1 <> (proc _ -> hitDyn -< (t2, note2))
      )
      [(0, Timed t1 note1), (t1 , Timed t2 note2)]

