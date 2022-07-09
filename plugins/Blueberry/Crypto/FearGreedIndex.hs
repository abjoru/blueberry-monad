{-# LANGUAGE OverloadedStrings #-}
module Blueberry.Crypto.FearGreedIndex where

import Xmobar

import Blueberry.Palette

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (withObject)
import Data.Text (Text, pack, unpack)

import Network.HTTP.Client.Conduit (Request, parseRequest)
import Network.HTTP.Simple (getResponseBody, httpJSON)

data FNG = FNG deriving (Read, Show)

instance Exec FNG where
  alias FNG = "fg"
  rate  FNG = 6000 -- 10 minutes
  run   FNG = runPlugin

-- name and data array
data Response a = Response String [a] deriving (Show, Eq)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \v -> 
    Response <$> v .: pack "name"
             <*> v .: pack "data"

data DataItem = DataItem String String deriving (Show, Eq)

instance FromJSON DataItem where
  parseJSON = withObject "DataItem" $ \v -> DataItem 
    <$> v .: "value"
    <*> v .: "value_classification"

runPlugin :: IO String
runPlugin = do
  maybeItem <- fetchIndex
  case maybeItem of
    Just (DataItem i v) -> return $ "<fc=" ++ indexColor i ++ ">\xf15a (" ++ i ++ ") " ++ v ++ "</fc>"
    Nothing             -> return $ "<fc=grey>\xf15a Fear/Greed Unknown!</fc>"

indexColor :: String -> String
indexColor num = case (read num :: Integer) of
                  x | x < 35 -> pRed0
                  x | x < 70 -> pYellow0
                  x          -> pGreen0

fetchIndex :: IO (Maybe DataItem)
fetchIndex = do
  request <- parseRequest "https://api.alternative.me/fng/"
  response <- callApi request
  return $ extr response
    where extr (Response _ (x:_)) = Just x
          extr _                  = Nothing

          callApi :: FromJSON b => Request -> IO (Response b)
          callApi r = do
            res <- httpJSON r
            pure $ getResponseBody res
