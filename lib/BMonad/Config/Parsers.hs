{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module BMonad.Config.Parsers (
  loadConfig,
  loadConfig',
  loadColorSchemes,
  loadColorSchemes',
  loadApplications,
  loadApplications'
) where

import           BMonad.Config.Types
import           BMonad.Utils        (capitalized, resolve)

import           Control.Arrow       (left)

import qualified Data.Aeson.KeyMap   as KM
import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (mapMaybe)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Data.Yaml

import           System.Directory    (XdgDirectory (XdgConfig),
                                      getHomeDirectory, getXdgDirectory)
import           System.FilePath     ((</>))

import           XMonad              (mod1Mask, mod4Mask)

-- |Load BMonad config files.
-- @param1 path to bmonad.yaml
-- @param2 path to themes.yaml (color schemes)
loadConfig :: FilePath -> FilePath -> IO Config
loadConfig cfg themes = do
  rcfg   <- decodeFileThrow cfg :: IO ReadConfig
  colors <- loadColorSchemes themes
  parseConfig (mkTheme (rcTheme rcfg) colors) rcfg
  where mkTheme (ReadTheme n f a b) cs = Theme (findSchemeWithDefault n cs) f a b

-- |Load BMonad config from byte strings
-- @param1 configuration byte string (i.e. bmonad.yaml)
-- @param2 color schemes byte string (i.e. themes.yaml)
loadConfig' :: B.ByteString -> B.ByteString -> IO Config
loadConfig' c t = do
  cfg <- either (fail . show) pure $ decodeEither' c
  thm <- either (fail . show) pure $ loadColorSchemes' t
  parseConfig (mkTheme (rcTheme cfg) thm) cfg
  where mkTheme (ReadTheme n f a b) cs = Theme (findSchemeWithDefault n cs) f a b

loadColorSchemes :: FilePath -> IO [(String, Scheme)]
loadColorSchemes file = do
  mobj <- decodeFileEither file :: IO (Either ParseException Value)
  return $ case mobj of
             Right v -> parseColorSchemes v
             Left er -> fail $ prettyPrintParseException er

loadColorSchemes' :: B.ByteString -> Either ParseException [(String, Scheme)]
loadColorSchemes' bs = parseColorSchemes <$> (decodeEither' bs :: Either ParseException Value)

loadApplications :: Config -> IO [Category]
loadApplications cfg = do
  yaml <- B.readFile $ cfgMonadDir cfg </> "applications.yaml"
  return $ case parseApplications yaml of
             Right v -> v
             _       -> []

loadApplications' :: B.ByteString -> IO [Category]
loadApplications' s = either fail pure $ parseApplications s

{-------------------------------------------
  Parsers
-------------------------------------------}

parseConfig :: Theme -> ReadConfig -> IO Config
parseConfig theme cfg = do
  hdir <- getHomeDirectory
  bdir <- getXdgDirectory XdgConfig "xmonad"
  gdir <- resolve (rcGameDir cfg) hdir

  return $ Config { cfgTheme = theme
                  , cfgMonadDir = bdir
                  , cfgGameDir = gdir
                  , cfgModMask = mod4Mask
                  , cfgAltMask = mod1Mask
                  , cfgTerminal = rcTerminal cfg
                  , cfgBrowser = rcBrowser cfg
                  , cfgEditor = rcEditor cfg
                  , cfgSoundPlayer = rcSound cfg
                  , cfgWorkspaces = rcWorkspaces cfg
                  , cfgLogLevel = rcLogLevel cfg
                  , cfgGridSelect = rcGridSelect cfg
                  , cfgMobarSettings = rcMobarSettings cfg
                  }

parseApplications :: B.ByteString -> Either String [Category]
parseApplications str = left prettyPrintParseException (decodeEither' str) >>= parseCategories

parseCategories :: Value -> Either String [Category]
parseCategories (Object o) = parseEither f (HM.toList $ KM.toHashMapText o)
  where f = mapM parseCategory
parseCategories _ = Left "Expected Yaml object for application list"

parseCategory :: (T.Text, Value) -> Parser Category
parseCategory (key, v) =
  let name = capitalized $ T.unpack key
      apps = parseApps v
   in Category name <$> apps

parseApps :: Value -> Parser [App]
parseApps (Array a) = mapM parseApp (V.toList a)
parseApps _         = fail "Expected Yaml array for app list!"

parseApp :: Value -> Parser App
parseApp = withObject "App" $ \v -> App
  <$> v .: "name"
  <*> v .: "description"
  <*> v .:? "command"
  <*> v .:? "terminal"

parseColorSchemes :: Value -> [(String, Scheme)]
parseColorSchemes (Object o) = mapMaybe parse (HM.toList (KM.toHashMapText o))
  where parse (k, v) = (T.unpack k,) <$> parseMaybe parseColorScheme v
parseColorSchemes _ = []

parseColorScheme :: Value -> Parser Scheme
parseColorScheme = withObject "Scheme" $ \v -> Scheme
  <$> v .: "colorbg"
  <*> v .: "colorfg"
  <*> v .: "color01"
  <*> v .: "color02"
  <*> v .: "color03"
  <*> v .: "color04"
  <*> v .: "color05"
  <*> v .: "color06"
  <*> v .: "color07"
  <*> v .: "color08"
  <*> v .: "color09"
  <*> v .: "color10"
  <*> v .: "color11"
  <*> v .: "color12"
  <*> v .: "color13"
  <*> v .: "color14"
  <*> v .: "color15"
  <*> v .: "color16"
