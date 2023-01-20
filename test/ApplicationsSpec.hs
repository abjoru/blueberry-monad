module ApplicationsSpec (main, spec) where

import           BMonad.Config.Parsers
import           BMonad.Config.Types
import           BMonad.Utils          (capitalized)

import           Data.ByteString       hiding (find, head, length, map)
import qualified Data.ByteString.Char8 as C
import           Data.List             (find)
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

grp :: String -> IO [App]
grp n = do
  maybeC <- find (\a -> categoryName a == capitalized n) <$> loadApplications' yaml
  return $ case maybeC of
             (Just (Category _ xs)) -> xs
             _                      -> []

spec :: Spec
spec = do
  describe "Applications" $ do
    it "parse application yaml" $ do
      a <- loadApplications' yaml
      length a `shouldBe` 2

    it "contains all apps" $ do
      a1 <- grp "games"
      a2 <- grp "internet"
      a1 `shouldBe` [App "Steam" "Game manager and store" (Just "steam") Nothing]
      a2 `shouldBe` [ App "QuteBrowser" "Lightweight vim-like web browser" (Just "qutebrowser") Nothing
                    , App "Links" "Commandline browser" Nothing (Just "links")
                    ]
