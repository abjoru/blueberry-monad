module BarSpec (main, spec) where

import           BMonad.Bar
import           BMonad.Config.Types (MobarWidget (..), Scheme (..))

import           Data.List           (isInfixOf)

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Test scheme
testScheme :: Scheme
testScheme = Scheme
  { colorBack = "#000000"
  , colorFore = "#ffffff"
  , color01   = "#111111"
  , color02   = "#222222"
  , color03   = "#333333"
  , color04   = "#444444"
  , color05   = "#555555"
  , color06   = "#666666"
  , color07   = "#777777"
  , color08   = "#888888"
  , color09   = "#999999"
  , color10   = "#aaaaaa"
  , color11   = "#bbbbbb"
  , color12   = "#cccccc"
  , color13   = "#dddddd"
  , color14   = "#eeeeee"
  , color15   = "#ffffff"
  , color16   = "#123456"
  }

spec :: Spec
spec = do
  describe "BMonad.Bar" $ do

    describe "constants" $ do
      it "icon contains xpm reference" $
        icon `shouldSatisfy` ("haskell_20.xpm" `isInfixOf`)

      it "icon contains icon tag" $
        icon `shouldSatisfy` ("<icon=" `isInfixOf`)

      it "defaultHeight is 24" $
        defaultHeight `shouldBe` 24

    describe "mkTemplate" $ do
      it "CoinPriceWidget -> %coinprices%" $
        mkTemplate CoinPriceWidget `shouldBe` Just "%coinprices%"

      it "FearGreedWidget -> %fgi%" $
        mkTemplate FearGreedWidget `shouldBe` Just "%fgi%"

      it "MemoryWidget -> %memory%" $
        mkTemplate MemoryWidget `shouldBe` Just "%memory%"

      it "DiskUWidget -> %disku%" $
        mkTemplate DiskUWidget `shouldBe` Just "%disku%"

      it "NetworkWidget -> %dynnetwork%" $
        mkTemplate NetworkWidget `shouldBe` Just "%dynnetwork%"

      it "DateWidget -> %date%" $
        mkTemplate DateWidget `shouldBe` Just "%date%"

      it "UpstreamWidget -> %upstream%" $
        mkTemplate UpstreamWidget `shouldBe` Just "%upstream%"

      it "TrayWidget -> %_XMONAD_TRAYPAD%" $
        mkTemplate TrayWidget `shouldBe` Just "%_XMONAD_TRAYPAD%"

      it "KeyboardWidget -> %kbd%" $
        mkTemplate KeyboardWidget `shouldBe` Just "%kbd%"

      it "UndefinedWidget -> Nothing" $
        mkTemplate UndefinedWidget `shouldBe` Nothing

    describe "mkWidget" $ do
      it "returns Just for CoinPriceWidget" $
        mkWidget testScheme CoinPriceWidget `shouldSatisfy` isJust'

      it "returns Just for FearGreedWidget" $
        mkWidget testScheme FearGreedWidget `shouldSatisfy` isJust'

      it "returns Just for MemoryWidget" $
        mkWidget testScheme MemoryWidget `shouldSatisfy` isJust'

      it "returns Just for DiskUWidget" $
        mkWidget testScheme DiskUWidget `shouldSatisfy` isJust'

      it "returns Just for NetworkWidget" $
        mkWidget testScheme NetworkWidget `shouldSatisfy` isJust'

      it "returns Just for DateWidget" $
        mkWidget testScheme DateWidget `shouldSatisfy` isJust'

      it "returns Just for UpstreamWidget" $
        mkWidget testScheme UpstreamWidget `shouldSatisfy` isJust'

      it "returns Just for TrayWidget" $
        mkWidget testScheme TrayWidget `shouldSatisfy` isJust'

      it "returns Just for KeyboardWidget" $
        mkWidget testScheme KeyboardWidget `shouldSatisfy` isJust'

      it "returns Nothing for UndefinedWidget" $
        mkWidget testScheme UndefinedWidget `shouldSatisfy` isNothing'

-- | Helper to check Maybe without importing Data.Maybe
isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' Nothing  = False

isNothing' :: Maybe a -> Bool
isNothing' = not . isJust'
