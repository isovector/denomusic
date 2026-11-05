module MusicTypesSpec where

import Data.Ratio ((%))
import FRP
import FRP.Primitives (observe)
import FRP.Types.Time (Time (..))
import Music.Types
import Test.Hspec

spec :: Spec
spec =
  describe "Horizontal sequencing" $
    it "does not promote a later hit through an initial rest" $ do
      let quarter = Time $ 1 % 4
          song = hit quarter 0 <> rest quarter <> hit quarter 1 <> rest quarter
      observe (fmap h_out $ unHorizontal song)
        `shouldBe` [(0, 0), (Time $ 1 % 2, 1)]
      observe (horizontal song)
        `shouldBe`
          [ (0, Span (Time $ 1 % 4) 0)
          , (Time $ 1 % 2, Span (Time $ 1 % 4) 1)
          ]
