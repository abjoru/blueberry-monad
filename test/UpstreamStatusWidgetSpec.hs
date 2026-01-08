module UpstreamStatusWidgetSpec (main, spec) where

import           BMonad.Bar.Plugin.UpstreamStatusWidget

import           Data.List                              (isInfixOf)

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Test settings with distinct colors
testSettings :: UpstreamSettings
testSettings = UpstreamSettings
  { upstreamRate        = 6000
  , upstreamAheadColor  = "#ahead"
  , upstreamBehindColor = "#behind"
  , upstreamSyncedColor = "#synced"
  }

spec :: Spec
spec = do
  describe "BMonad.Bar.Plugin.UpstreamStatusWidget" $ do

    describe "formatOutput" $ do

      describe "ahead status" $ do
        it "shows +N when ahead" $ do
          let result = formatOutput testSettings 5 0
          result `shouldSatisfy` ("+5" `isInfixOf`)

        it "uses ahead color when ahead" $ do
          let result = formatOutput testSettings 3 0
          result `shouldSatisfy` ("#ahead" `isInfixOf`)

        it "shows +1 for single commit ahead" $ do
          let result = formatOutput testSettings 1 0
          result `shouldSatisfy` ("+1" `isInfixOf`)

      describe "behind status" $ do
        it "shows -N when behind" $ do
          let result = formatOutput testSettings 0 5
          result `shouldSatisfy` ("-5" `isInfixOf`)

        it "uses behind color when behind" $ do
          let result = formatOutput testSettings 0 3
          result `shouldSatisfy` ("#behind" `isInfixOf`)

        it "shows -1 for single commit behind" $ do
          let result = formatOutput testSettings 0 1
          result `shouldSatisfy` ("-1" `isInfixOf`)

      describe "synced status" $ do
        it "uses synced color when synced" $ do
          let result = formatOutput testSettings 0 0
          result `shouldSatisfy` ("#synced" `isInfixOf`)

        it "does not show +/- when synced" $ do
          let result = formatOutput testSettings 0 0
          result `shouldSatisfy` (not . ("+" `isInfixOf`))
          result `shouldSatisfy` (not . ("-" `isInfixOf`))

      describe "priority" $ do
        it "prioritizes ahead over behind" $ do
          let result = formatOutput testSettings 2 3
          result `shouldSatisfy` ("+2" `isInfixOf`)
          result `shouldSatisfy` ("#ahead" `isInfixOf`)

      describe "icon presence" $ do
        it "always includes git icon glyph" $ do
          formatOutput testSettings 0 0 `shouldSatisfy` ("\xf418" `isInfixOf`)
          formatOutput testSettings 5 0 `shouldSatisfy` ("\xf418" `isInfixOf`)
          formatOutput testSettings 0 5 `shouldSatisfy` ("\xf418" `isInfixOf`)
