{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module CryptoSpec (main, spec) where

import           BMonad.Crypto

import           Data.Aeson           (FromJSON (..), eitherDecode, withObject,
                                       (.:))
import qualified Data.ByteString.Lazy as LBS

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Mock CoinGecko API response for coin prices
mockCoinPricesJson :: LBS.ByteString
mockCoinPricesJson = "[{\"id\":\"bitcoin\",\"symbol\":\"btc\",\"name\":\"Bitcoin\",\"image\":\"https://example.com/btc.png\",\"current_price\":42000.50},{\"id\":\"ethereum\",\"symbol\":\"eth\",\"name\":\"Ethereum\",\"image\":\"https://example.com/eth.png\",\"current_price\":2500.75}]"

-- | Mock single coin response
mockSingleCoinJson :: LBS.ByteString
mockSingleCoinJson = "[{\"id\":\"cardano\",\"symbol\":\"ada\",\"name\":\"Cardano\",\"image\":\"https://example.com/ada.png\",\"current_price\":0.45}]"

-- | Mock Fear & Greed API response
mockFearGreedJson :: LBS.ByteString
mockFearGreedJson = "{\"name\":\"Fear and Greed Index\",\"data\":[{\"value\":\"65\",\"value_classification\":\"Greed\",\"timestamp\":\"1234567890\"}]}"

-- | Mock Fear & Greed with extreme fear
mockFearGreedLowJson :: LBS.ByteString
mockFearGreedLowJson = "{\"name\":\"Fear and Greed Index\",\"data\":[{\"value\":\"15\",\"value_classification\":\"Extreme Fear\",\"timestamp\":\"1234567890\"}]}"

-- | Mock empty response
mockEmptyDataJson :: LBS.ByteString
mockEmptyDataJson = "{\"name\":\"Fear and Greed Index\",\"data\":[]}"

spec :: Spec
spec = do
  describe "BMonad.Crypto.CoinPrice" $ do

    describe "JSON parsing" $ do
      it "parses multiple coins from JSON" $ do
        let result = eitherDecode mockCoinPricesJson :: Either String [Coin]
        case result of
          Right coins -> length coins `shouldBe` 2
          Left err    -> expectationFailure err

      it "extracts coin id correctly" $ do
        let Right coins = eitherDecode mockCoinPricesJson :: Either String [Coin]
        coinId (head coins) `shouldBe` "bitcoin"
        coinId (coins !! 1) `shouldBe` "ethereum"

      it "extracts coin name correctly" $ do
        let Right coins = eitherDecode mockCoinPricesJson :: Either String [Coin]
        coinName (head coins) `shouldBe` "Bitcoin"
        coinName (coins !! 1) `shouldBe` "Ethereum"

      it "extracts coin price correctly" $ do
        let Right coins = eitherDecode mockCoinPricesJson :: Either String [Coin]
        coinPriceUsd (head coins) `shouldBe` 42000.50
        coinPriceUsd (coins !! 1) `shouldBe` 2500.75

      it "extracts image URI correctly" $ do
        let Right coins = eitherDecode mockCoinPricesJson :: Either String [Coin]
        coinImageUri (head coins) `shouldBe` "https://example.com/btc.png"

      it "parses single coin response" $ do
        let Right coins = eitherDecode mockSingleCoinJson :: Either String [Coin]
        length coins `shouldBe` 1
        coinId (head coins) `shouldBe` "cardano"
        coinPriceUsd (head coins) `shouldBe` 0.45

      it "handles fractional prices" $ do
        let Right coins = eitherDecode mockSingleCoinJson :: Either String [Coin]
        coinPriceUsd (head coins) `shouldBe` 0.45

  describe "BMonad.Crypto.FearGreed" $ do

    describe "JSON parsing" $ do
      it "parses fear/greed response" $ do
        let result = parseFearGreed mockFearGreedJson
        result `shouldSatisfy` isRight

      it "extracts index value correctly" $ do
        let Right (Just (FearGreed val _)) = parseFearGreed mockFearGreedJson
        val `shouldBe` 65

      it "extracts classification correctly" $ do
        let Right (Just (FearGreed _ cls)) = parseFearGreed mockFearGreedJson
        cls `shouldBe` "Greed"

      it "parses extreme fear values" $ do
        let Right (Just (FearGreed val cls)) = parseFearGreed mockFearGreedLowJson
        val `shouldBe` 15
        cls `shouldBe` "Extreme Fear"

    describe "Coin equality" $ do
      it "equal coins are equal" $ do
        let coin1 = Coin "btc" "Bitcoin" "img" 100.0
            coin2 = Coin "btc" "Bitcoin" "img" 100.0
        coin1 `shouldBe` coin2

      it "different prices make coins different" $ do
        let coin1 = Coin "btc" "Bitcoin" "img" 100.0
            coin2 = Coin "btc" "Bitcoin" "img" 200.0
        coin1 `shouldNotBe` coin2

-- | Helper to parse FearGreed from the nested response structure
parseFearGreed :: LBS.ByteString -> Either String (Maybe FearGreed)
parseFearGreed bs = extractFirst <$> eitherDecode bs
  where
    extractFirst :: FGResponse -> Maybe FearGreed
    extractFirst (FGResponse _ (x:_)) = Just x
    extractFirst _                    = Nothing

-- | Internal response type for testing
data FGResponse = FGResponse String [FearGreed]

instance FromJSON FGResponse where
  parseJSON = withObject "FGResponse" $ \v -> FGResponse
    <$> v .: "name"
    <*> v .: "data"

-- | Helper
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
