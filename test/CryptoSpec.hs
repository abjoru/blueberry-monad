module CryptoSpec (main, spec) where

import           BMonad.Crypto

import           Data.Maybe    (isJust)

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Crypto" $ do
    it "fetches fear/greed index" $ do
      maybeIndex <- getFearGreedIndex
      maybeIndex `shouldSatisfy` isJust

    it "fetches one coin price" $ do
      ps <- getCoinPrices ["bitcoin"]
      coinId (head ps) `shouldBe` "bitcoin"
      coinName (head ps) `shouldBe` "Bitcoin"

    it "fetches multiple coin prices" $ do
      ps <- getCoinPrices ["bitcoin", "ethereum"]
      putStrLn $ show ps
      length ps `shouldBe` 2
