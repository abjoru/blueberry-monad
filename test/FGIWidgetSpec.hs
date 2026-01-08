module FGIWidgetSpec (main, spec) where

import           BMonad.Bar.Plugin.FGIWidget

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Test settings with distinct colors for easy verification
testSettings :: FGISettings
testSettings = FGISettings "HIGH" "MED" "LOW"

spec :: Spec
spec = do
  describe "BMonad.Bar.Plugin.FGIWidget" $ do

    describe "indexColor" $ do

      describe "low threshold (< 35)" $ do
        it "returns low color for 0" $
          indexColor testSettings 0 `shouldBe` "LOW"

        it "returns low color for 34" $
          indexColor testSettings 34 `shouldBe` "LOW"

        it "returns low color for negative values" $
          indexColor testSettings (-10) `shouldBe` "LOW"

      describe "medium threshold (35 <= x < 70)" $ do
        it "returns med color for 35 (boundary)" $
          indexColor testSettings 35 `shouldBe` "MED"

        it "returns med color for 50 (middle)" $
          indexColor testSettings 50 `shouldBe` "MED"

        it "returns med color for 69 (upper bound)" $
          indexColor testSettings 69 `shouldBe` "MED"

      describe "high threshold (>= 70)" $ do
        it "returns high color for 70 (boundary)" $
          indexColor testSettings 70 `shouldBe` "HIGH"

        it "returns high color for 85" $
          indexColor testSettings 85 `shouldBe` "HIGH"

        it "returns high color for 100" $
          indexColor testSettings 100 `shouldBe` "HIGH"

      describe "boundary transitions" $ do
        it "34 -> 35 transitions from low to med" $ do
          indexColor testSettings 34 `shouldBe` "LOW"
          indexColor testSettings 35 `shouldBe` "MED"

        it "69 -> 70 transitions from med to high" $ do
          indexColor testSettings 69 `shouldBe` "MED"
          indexColor testSettings 70 `shouldBe` "HIGH"
