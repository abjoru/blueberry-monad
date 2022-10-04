{-# LANGUAGE OverloadedStrings #-}
module BMonad.Bar.Plugin.FearGreed where

import           Xmobar

import           Data.Aeson                  (FromJSON, parseJSON, (.:))
import           Data.Aeson.Types            (withObject)

import           Network.HTTP.Client.Conduit (Request, parseRequest)
import           Network.HTTP.Simple         (getResponseBody, httpJSON)

data FearGreed = FearGreed
  { colorHigh :: String
  , colorMed  :: String
  , colorLow  :: String
  } deriving (Read, Show)

instance Exec FearGreed where
  alias _                 = "fg"
  rate  _                 = 6000 -- 10 minutes
  run   (FearGreed h m l) = runPlugin h m l

-- name and data array
data Response a = Response String [a] deriving (Show, Eq)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \v ->
    Response <$> v .: "name"
             <*> v .: "data"

data DataItem = DataItem String String deriving (Show, Eq)

instance FromJSON DataItem where
  parseJSON = withObject "DataItem" $ \v ->
    DataItem <$> v .: "value"
             <*> v .: "value_classification"

runPlugin :: String -> String -> String -> IO String
runPlugin h m l = do
  maybeItem <- fetchIndex
  case maybeItem of
    Just (DataItem i v) -> return $ "<fc=" ++ indexColor h m l i ++ ">\xf15a (" ++ i ++ ") " ++ v ++ "</fc>"
    Nothing             -> return $ "<fc=grey>\xf15a Fear/Greed Unknown!</fc>"

indexColor :: String -> String -> String -> String -> String
indexColor high med low num = case (read num :: Integer) of
                     x | x < 35 -> low
                     x | x < 70 -> med
                     x          -> high

fetchIndex :: IO (Maybe DataItem)
fetchIndex = do
  request  <- parseRequest "https://api.alternative.me/fng/"
  response <- callApi request
  return $ extr response
    where extr (Response _ (x:_)) = Just x
          extr _                  = Nothing

          callApi :: FromJSON b => Request -> IO (Response b)
          callApi r = do
            res <- httpJSON r
            pure $ getResponseBody res
