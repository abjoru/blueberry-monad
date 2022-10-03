{-# LANGUAGE OverloadedStrings #-}
module BMonad.Applications (
  BApp(..),
  BCategory(..),

  loadApplications,
  parseApplications,
  applicationList,
  applicationMap,
  findByCategory,
  appLauncher,
  gridSelect,
  gridSelect'
  ) where

import           BMonad.Utils        (capitalized)
import           BMonad.Variables    (myTerminal, myXMonadDir)

import           Control.Monad       as MO

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import           Data.List           (find)
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Data.Yaml

import           System.FilePath     ((</>))

data BApp = BApp
  { bAppName     :: String
  , bAppDesc     :: String
  , bAppCommand  :: Maybe String
  , bAppTerminal :: Maybe String
  } deriving (Show, Eq)

data BCategory = BCategory
  { bCategoryName :: String
  , bApplications :: [BApp]
  } deriving (Show, Eq)

-- | Load applications from disk.
-- Config file location is ~/.config/xmonad/applications.yaml.
loadApplications :: IO [BCategory]
loadApplications = do
  xdir <- myXMonadDir
  yaml <- B.readFile $ xdir </> "applications.yaml"
  return $ case (parseApplications yaml) of
             (Right v) -> v
             (Left _)  -> []

{--------------------------------------------------------------------
  Data parsers
--------------------------------------------------------------------}

-- | Parses application config file contents.
-- Returns either an error message or list of application categories.
parseApplications :: B.ByteString -> Either String [BCategory]
parseApplications s = case yaml of
                        (Right a) -> parseEither parseCategories a
                        (Left er) -> Left $ prettyPrintParseException er
  where yaml = decodeEither' s :: Either ParseException Value

-- | Application categories parser
parseCategories :: Value -> Parser [BCategory]
parseCategories (Object o) = MO.mapM parseCategory (HM.toList o)

-- | Application category parser
parseCategory :: (T.Text, Value) -> Parser BCategory
parseCategory (key, a) =
  let name = capitalized $ T.unpack key
      apps = parseApps a
   in BCategory name <$> apps

-- | Applications parser
parseApps :: Value -> Parser [BApp]
parseApps (Array a) = MO.mapM parseApp (V.toList a)

-- | Application parser
parseApp :: Value -> Parser BApp
parseApp (Object o) = BApp
  <$> o .:  "name"
  <*> o .:  "description"
  <*> o .:? "command"
  <*> o .:? "terminal"

{--------------------------------------------------------------------
  Operations
--------------------------------------------------------------------}

-- | Flattens list of categories into list of applications.
applicationList :: [BCategory] -> [BApp]
applicationList = foldr (\c acc -> acc ++ (bApplications c)) []

-- | Flattens list of categories into application map.
-- Map is keyed off of application name as defined in config file.
applicationMap :: [BCategory] -> M.Map String BApp
applicationMap xs = M.fromList $ map (\x -> (bAppName x, x)) (applicationList xs)

-- | Find a category by name (case insensitive).
findByCategory :: String -> [BCategory] -> [BApp]
findByCategory c cs = case e of
                        Just (BCategory _ apps) -> apps
                        _                       -> []
  where e = find (\a -> (bCategoryName a) == (capitalized c)) cs

-- | Generate application launcher.
appLauncher :: BApp -> String
appLauncher (BApp _ _ (Just cmd) _) = cmd
appLauncher (BApp _ _ _ (Just cmd)) = myTerminal ++ " -e " ++ cmd

gridSelect :: BCategory -> [(String, String)]
gridSelect (BCategory _ apps) = map (\a -> ((bAppName a), (appLauncher a))) apps

gridSelect' :: String -> [BCategory] -> [(String, String)]
gridSelect' s cs = map f $ findByCategory s cs
  where f a = (bAppName a, appLauncher a)
