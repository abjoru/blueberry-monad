module CoinPriceWidgetSpec (main, spec) where

import           BMonad.Bar.Plugin.CoinPriceWidget
import           BMonad.Crypto.CoinPrice           (Coin (..))

import           Data.List                         (isInfixOf)

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Test coin settings
testSettings :: CoinSettings
testSettings = CoinSettings "bitcoin" "#ff9900" "\xf15a"

-- | Test coin data
testCoin :: Coin
testCoin = Coin
  { coinId       = "bitcoin"
  , coinName     = "Bitcoin"
  , coinImageUri = "https://example.com/btc.png"
  , coinPriceUsd = 42123.45
  }

spec :: Spec
spec = do
  describe "BMonad.Bar.Plugin.CoinPriceWidget" $ do

    describe "formatCoin" $ do
      it "wraps output in color tags" $ do
        let result = formatCoin (testSettings, testCoin)
        result `shouldSatisfy` ("<fc=#ff9900>" `isInfixOf`)
        result `shouldSatisfy` ("</fc>" `isInfixOf`)

      it "includes icon in font tags" $ do
        let result = formatCoin (testSettings, testCoin)
        result `shouldSatisfy` ("<fn=1>" `isInfixOf`)
        result `shouldSatisfy` ("</fn>" `isInfixOf`)

      it "includes icon glyph" $ do
        let result = formatCoin (testSettings, testCoin)
        result `shouldSatisfy` ("\xf15a" `isInfixOf`)

      it "includes dollar sign before price" $ do
        let result = formatCoin (testSettings, testCoin)
        result `shouldSatisfy` ("$" `isInfixOf`)

      it "formats price with 2 decimal places" $ do
        let result = formatCoin (testSettings, testCoin)
        result `shouldSatisfy` ("42123.45" `isInfixOf`)

      it "uses color from settings" $ do
        let settings = CoinSettings "eth" "#627eea" "E"
            coin = testCoin { coinId = "eth" }
            result = formatCoin (settings, coin)
        result `shouldSatisfy` ("<fc=#627eea>" `isInfixOf`)

      it "handles small prices correctly" $ do
        let coin = testCoin { coinPriceUsd = 0.05 }
            result = formatCoin (testSettings, coin)
        result `shouldSatisfy` ("0.05" `isInfixOf`)

      it "handles large prices correctly" $ do
        let coin = testCoin { coinPriceUsd = 123456.78 }
            result = formatCoin (testSettings, coin)
        result `shouldSatisfy` ("123456.78" `isInfixOf`)
