module BMonad.Applications (
  BApp(..),
  BCategory(..),

  loadApplications,
  applicationList,
  applicationMap,
  findByCategory,
  appLauncher
  ) where

import BMonad.Variables (myTerminal, myXMonadDir)
import BMonad.Utils (capitalized)

import Data.YAML.Aeson (decodeValue)
import Data.HashMap.Strict as HM
import Data.Text as T
import qualified Data.ByteString.Lazy as B
import System.FilePath ((</>))

data BApp = BApp
  { bAppName     :: String
  , bAppDesc     :: String
  , bAppCommand  :: Maybe String
  , bAppTerminal :: Maybe String
  } deriving Show, Eq

data BCategory = BCategory
  { bCategoryName :: String
  , bApplications :: [BApp]
  } deriving Show, Eq

instance FromJSON BCategory where
  parseJSON = withObject "BCategory" parseCategories

loadApplications :: IO (Either (Pos, String) [BCategory])
loadApplications = do
  cd <- myXMonadDir
  bs <- B.readFile $ cd </> "applications.yaml"
  return $ decode1 bs

parseCategories :: HM.HashMap T.Text Value -> Parser [BCategory]
parseCategories m = mapM parseCategory (HM.toList m)

parseCategory :: (T.Text, Value) -> Parser BCategory
parseCategory (key, Object o) =
  let name = capitalized $ T.unpack key
      apps = parseApps o
   in BCategory name <$> apps

parseApps :: HM.HashMap T.Text Value -> Parser [BApp]
parseApps m = mapM parseApp (HM.toList m)

parseApp :: (T.Text, Value) -> Parser BApp
parseApp (key, Object o) =
  let name = parseJSON $ HM.findWithDefault (capitalized key) "name" o
      desc = parseJSON $ HM.findWithDefault "Missing description!" "description" o
      cmd  = parseJSON $ HM.lookup "command" o
      term = parseJSON $ HM.lookup "terminal" o
   in BApp name <$> desc
                <*> cmd
                <*> term

applicationList :: [BCategory] -> [BApp]
applicationList = foldr (\c acc -> acc ++ (bApplications c))

applicationMap :: [BCategory] -> Map String BApp
applicationMap = fromList $ map (\x -> (bAppName x, x)) $ applicationList

findByCategory :: String -> [BCategory] -> [BApp]
findByCategory c cs = case found of
                        Just [] -> []
                        Nothing -> []
                        Just ca -> bApplications ca
  where found = find (\a -> (bCategoryName a) eq (capitalized c)) cs

appLauncher :: BApp -> String
appLauncher BApp _ _ (Just cmd) _ = cmd
appLauncher BApp _ _ _ (Just cmd) = myTerminal ++ " -e " ++ cmd
