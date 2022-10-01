module ApplicationsSpec where

import           BMonad.Applications

import           Control.Monad
import           Data.ByteString       hiding (head, length)
import qualified Data.ByteString.Char8 as C
import           Data.Either           (isRight)
import           Data.Map              ((!))
import           Test.Hspec

main :: IO ()
main = hspec spec

yaml :: ByteString
yaml = C.pack $ unlines [ "games:",
                          " - steam:",
                          "   name: 'Steam'",
                          "   description: 'Game manager and store'",
                          "   command: 'steam'",
                          "",
                          "internet:",
                          " - qutebrowser:",
                          "   name: 'QuteBrowser'",
                          "   description: 'Lightweight vim-like web browser'",
                          "   command: 'qutebrowser'",
                          " - links:",
                          "   name: 'Links'",
                          "   description: 'Commandline browser'",
                          "   terminal: 'links'"
                        ]

apps :: Either String [BCategory]
apps = parseApplications yaml

spec :: Spec
spec = do
  describe "Applications" $ do
    it "parse application yaml" $ do
      apps `shouldSatisfy` isRight
      case apps of
        (Right a) -> (length a) `shouldBe` 2
        (Left er) -> expectationFailure "Failed to parse input!"

    it "create application list" $ do
      case apps of
        (Right a) -> (length $ applicationList a) `shouldBe` 3
        (Left er) -> expectationFailure "Failed to create application list!"

    it "find applications by category" $ do
      case apps of
        (Right a) -> (length $ findByCategory "internet" a) `shouldBe` 2
        (Left _)  -> expectationFailure "Failed to find by category!"

    it "generate launcher command" $ do
      case apps of
        (Right a) -> (appLauncher ((applicationMap a) ! "Steam")) `shouldBe` "steam"
        (Left _)  -> expectationFailure "Failed to generate launcher!"

    it "generate launcher terminal command" $ do
      case apps of
        (Right a) -> (appLauncher ((applicationMap a) ! "Links")) `shouldBe` "alacritty -e links"
        (Left _)  -> expectationFailure "Failed to generate launcher!"

    it "generates grid select config" $ do
      case apps of
        (Right a) -> (gridSelect $ head a) `shouldBe` [("Steam", "steam")]
        (Left _)  -> expectationFailure "Failed to generate grid select config!"
