{-# LANGUAGE OverloadedStrings #-}
module BMonad.Crypto.CoinPrice (
  Symbol,
  Coin(..),
  getCoinPrices
) where

import           Data.Aeson          (FromJSON (parseJSON), withObject, (.:))
import           Data.List           (intercalate)

import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

-- |Represents CoinGecko coin Ids.
type Symbol = String

-- |CoinGecko coin data.
data Coin = Coin
  { coinId       :: Symbol
  , coinName     :: String
  , coinImageUri :: String
  , coinPriceUsd :: Double
  } deriving (Show, Eq)

instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \v -> Coin
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "image"
    <*> v .: "current_price"

mkUri :: [Symbol] -> String
mkUri xs = "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=" ++ intercalate "," xs

getCoinPrices :: [Symbol] -> IO [Coin]
getCoinPrices xs = getResponseBody <$> (parseRequest (mkUri xs) >>= httpJSON)
