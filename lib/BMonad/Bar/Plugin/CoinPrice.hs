{-# LANGUAGE OverloadedStrings #-}
module BMonad.Bar.Plugin.CoinPrice where

import           Xmobar

import           Data.Aeson
import           Data.Aeson.Types
import           Data.List

import           Text.Printf

import           Network.HTTP.Client.Conduit
import           Network.HTTP.Simple         (getResponseBody, httpJSON)

data Coin = Coin
  { coinId            :: String
  , coinColor         :: String
  , coinNerdFrontCode :: String
  } deriving (Read, Show)

data CoinConfig = CoinConfig
  { cfgCoins       :: [Coin]
  , cfgSepColor    :: String
  , cfgRefreshRate :: Int
  } deriving (Read, Show)

instance Exec CoinConfig where
  alias _                 = "coinprice"
  rate (CoinConfig _ _ r) = r
  run  (CoinConfig c s _) = do
    symbols <- fetchData c
    return $ intercalate ("<fc=" ++ s ++ "> | </fc>") $ mkWidget c symbols

data Symbol = Symbol
  { symbolId    :: String
  , symbolName  :: String
  , symbolImage :: String
  , symbolPrice :: Double
  } deriving (Show, Eq)

instance FromJSON Symbol where
  parseJSON = withObject "Symbol" $ \v ->
    Symbol <$> v .: "id"
           <*> v .: "name"
           <*> v .: "image"
           <*> v .: "current_price"

callApi :: FromJSON a => Request -> IO [a]
callApi r = do
  response <- httpJSON r
  pure $ getResponseBody response

mkUrl :: [Coin] -> String
mkUrl xs =
  let ids = intercalate "," $ map coinId xs
   in "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=" ++ ids

mkWidget :: [Coin] -> [Symbol] -> [String]
mkWidget cs xs = map f cs
  where f (Coin i c fc) = case (find (\e -> symbolId e == i) xs) of
                            Just (Symbol _ _ _ p) -> formatWidget c fc (Just p)
                            Nothing               -> formatWidget c fc Nothing

formatWidget :: String -> String -> Maybe Double -> String
formatWidget color icon (Just price) = "<fc=" ++ color ++ ">" ++ icon ++ " $" ++ (printf "%.2f" price) ++ "</fc>"
formatWidget color icon Nothing      = "<fc=" ++ color ++ ">" ++ icon ++ " N/A</fc>"

fetchData :: [Coin] -> IO [Symbol]
fetchData cs = do
  request  <- parseRequest $ mkUrl cs
  response <- callApi request
  return response
