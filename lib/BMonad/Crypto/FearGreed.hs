{-# LANGUAGE OverloadedStrings #-}
module BMonad.Crypto.FearGreed (FearGreed(..), getFearGreedIndex) where

import           Data.Aeson                  (FromJSON, parseJSON, (.:))
import           Data.Aeson.Types            (withObject)

import           Network.HTTP.Client.Conduit (parseRequest)
import           Network.HTTP.Simple         (getResponseBody, httpJSON)

apiUrl :: String
apiUrl = "https://api.alternative.me/fng/"

-- |Data item for fear and greed index.
-- @param1 numeric value as string
-- @param2 value classification string
data FearGreed = FearGreed Integer String deriving (Show, Eq)

instance FromJSON FearGreed where
  parseJSON = withObject "FearGreed" $ \v -> FearGreed
    <$> fmap read (v .: "value")
    <*> v .: "value_classification"

-- |Data API response type
-- @param1 name
-- @param2 array of fear/greed index
data Response a = Response String [a]

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \v -> Response
    <$> v .: "name"
    <*> v .: "data"

getFearGreedIndex :: IO (Maybe FearGreed)
getFearGreedIndex = extr . getResponseBody <$> (parseRequest apiUrl >>= httpJSON)
  where extr (Response _ (h:_)) = Just h
        extr _                  = Nothing
