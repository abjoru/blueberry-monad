{-# LANGUAGE OverloadedStrings #-}

module BMonad.Crypto.CoinPrice
  ( Symbol,
    Coin (..),
    getCoinPrices,
  )
where

import           Control.Monad.Catch
import           Data.Aeson                (FromJSON (parseJSON), withObject,
                                            (.:))
import           Data.List                 (intercalate)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header

-- | Represents CoinGecko coin Ids.
type Symbol = String

-- | CoinGecko coin data.
data Coin = Coin
  { coinId       :: Symbol,
    coinName     :: String,
    coinImageUri :: String,
    coinPriceUsd :: Double
  }
  deriving (Show, Eq)

instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \v ->
    Coin
      <$> v
      .: "id"
      <*> v
      .: "name"
      <*> v
      .: "image"
      <*> v
      .: "current_price"

mkUri :: [Symbol] -> String
mkUri xs = "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=" ++ intercalate "," xs

mkRequest :: MonadThrow m => [Symbol] -> m Request
mkRequest xs = do
  req <- parseRequestThrow $ mkUri xs
  return (setRequestHeader hUserAgent ["curl/8.0.1"] req)

getCoinPrices :: [Symbol] -> IO [Coin]
getCoinPrices xs = getResponseBody <$> (mkRequest xs >>= httpJSON)
