{-# LANGUAGE OverloadedStrings #-}
module UtilsSpec (main, spec) where

import           BMonad.Utils

import           System.Directory
import           System.FilePath
import           System.Posix.User (getLoginName)

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Utils" $ do
    it "resolves home alias" $ do
      hdir <- getHomeDirectory
      user <- getLoginName
      dir1 <- resolve "~/Games" hdir
      dir2 <- resolve ("/home/" ++ user) hdir
      dir1 `shouldBe` hdir </> "Games"
      dir2 `shouldBe` hdir

    it "capitalize strings" $ do
      capitalized "test" `shouldBe` "Test"
      capitalized "tEST" `shouldBe` "Test"

    it "converts HEX to RGB" $ do
      hexToRgb "#282828" `shouldBe` (40, 40, 40)
      hexToRgb "#ebdbb2" `shouldBe` (235, 219, 178)
