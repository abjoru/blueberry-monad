module BMonad.Config (
  Font,
  Color,
  Scheme(..),
  Theme(..),
  Config(..),
  MobarWidget(..),
  MobarSettings(..),
  GridSettings(..),

  bmonadConfig,
  bmonadApps,
  bmonadAppGroup,
  bmonadGames,
  bmonadGridConfig,
  bmonadTabTheme,
  bmonadStartupHook,

  mobarAlpha,
  mobarFgColor,
  mobarBgColor,
  mobarBorderColor,
  mobarSingleMonitor,
  mobarPrimaryMonitor,
  mobarSecondaryMonitor,
  mobarOtherMonitors,

  spawnSelected,
  spawnSelectedIO
) where

import           BMonad.Config.Parsers     (loadApplications, loadConfig)
import           BMonad.Config.Types
import           BMonad.Log                (warnX)
import           BMonad.Utils

import           Data.List                 (find)
import qualified Data.Map                  as M
import           Data.Maybe                (catMaybes)

import           System.Directory          (XdgDirectory (XdgConfig),
                                            doesDirectoryExist, getXdgDirectory)
import           System.FilePath           (takeBaseName, takeExtension, (</>))

import           Control.Monad             (filterM)

import           UnliftIO.Exception        (catchAny)

import           XMonad                    (Window, X, io, spawn, whenJust)
import           XMonad.Actions.GridSelect (GSConfig (..), buildDefaultGSConfig,
                                            colorRangeFromClassName, def,
                                            gridselect)
import           XMonad.Hooks.SetWMName    (setWMName)
import qualified XMonad.Layout.Tabbed      as T
import           XMonad.Util.SpawnOnce     (spawnOnce)

{----------------------------------------
  Operations
----------------------------------------}

bmonadConfig :: IO Config
bmonadConfig = do
  bdir <- getXdgDirectory XdgConfig "xmonad"
  let a = bdir </> "bmonad.yaml"
      b = bdir </> "themes.yaml"
   in catchAny (loadConfig a b) handler
  where handler er = warnX "LoadConfig" (show er) >> defaultConfig

bmonadStartupHook :: Config -> X ()
bmonadStartupHook cfg = do
  spawnOnce $ cfgMonadDir cfg </> "autostart.sh"
  setWMName "XBlueberry"

bmonadApps :: Config -> IO (M.Map String [(String, String)])
bmonadApps cfg = do
  apps <- catchAny (loadApplications cfg) handler
  return $ M.fromList $ map tuples apps
  where handler er = warnX "LoadApps" (show er) >> pure []
        tuples cat = (lowerCase $ categoryName cat, gridSelect cfg cat)

-- Case insensitive match
bmonadAppGroup :: Config -> String -> IO [(String, String)]
bmonadAppGroup cfg c = M.findWithDefault [] (lowerCase c) <$> bmonadApps cfg

bmonadGames :: Config -> IO [(String, String)]
bmonadGames cfg = do
  gdirs   <- listAbsDir $ cfgGameDir cfg
  rdirs   <- filterM exists gdirs
  results <- mapM findGameLauncher rdirs
  return $ map (\a -> (takeBaseName a, show a)) $ catMaybes results
  where exists = doesDirectoryExist . (cfgGameDir cfg </>)

bmonadGridConfig :: Config -> GSConfig Window
bmonadGridConfig cfg = (buildDefaultGSConfig $ col (themeColorScheme $ cfgTheme cfg))
  { gs_cellheight   = gsCellHeight $ cfgGridSelect cfg
  , gs_cellwidth    = gsCellWidth $ cfgGridSelect cfg
  , gs_cellpadding  = gsCellPadding $ cfgGridSelect cfg
  , gs_originFractX = gsOriginFractX $ cfgGridSelect cfg
  , gs_originFractY = gsOriginFractY $ cfgGridSelect cfg
  , gs_font         = themeFont $ cfgTheme cfg
  }
  where col scheme = colorRangeFromClassName
                       (hexToRgb $ colorBack scheme) -- ^ beginning of color range
                       (hexToRgb $ color13 scheme)   -- ^ end of color range
                       (hexToRgb $ color09 scheme)   -- ^ bg of the active window
                       (hexToRgb $ color04 scheme)   -- ^ inactive text color
                       (hexToRgb $ colorFore scheme) -- ^ active text color

bmonadTabTheme :: Config -> T.Theme
bmonadTabTheme cfg = T.def
  { T.fontName            = themeFont $ cfgTheme cfg
  , T.activeColor         = color15 . themeColorScheme $ cfgTheme cfg
  , T.inactiveColor       = color08 . themeColorScheme $ cfgTheme cfg
  , T.activeBorderColor   = color15 . themeColorScheme $ cfgTheme cfg
  , T.inactiveBorderColor = colorBack . themeColorScheme $ cfgTheme cfg
  , T.activeTextColor     = colorBack . themeColorScheme $ cfgTheme cfg
  , T.inactiveTextColor   = color16 . themeColorScheme $ cfgTheme cfg
  }

spawnSelected :: Config -> [(String, String)] -> X ()
spawnSelected cfg lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def { gs_cellheight   = gsCellHeight $ cfgGridSelect cfg
                   , gs_cellwidth    = gsCellWidth $ cfgGridSelect cfg
                   , gs_cellpadding  = gsCellPadding $ cfgGridSelect cfg
                   , gs_originFractX = gsOriginFractX $ cfgGridSelect cfg
                   , gs_originFractY = gsOriginFractY $ cfgGridSelect cfg
                   , gs_font         = themeFont $ cfgTheme cfg
                   }

spawnSelectedIO :: Config -> IO [(String, String)] -> X ()
spawnSelectedIO cfg lst = io lst >>= spawnSelected cfg

{----------------------------------------
  Utilities
----------------------------------------}

findGameLauncher :: FilePath -> IO (Maybe FilePath)
findGameLauncher parent = do
  cx <- listAbsDir parent
  checkExecutable $ find (\a -> ".sh" == takeExtension a) cx

gridSelect :: Config -> Category -> [(String, String)]
gridSelect c (Category _ apps) = map (\a -> (appName a, mkLauncher c a)) apps
  where mkLauncher _ (App _ _ (Just cmd) _) = cmd
        mkLauncher r (App _ _ _ (Just cmd)) = cfgTerminal r ++ " -e " ++ cmd
        mkLauncher _ (App n _ _ _)          = "notify-send \"" ++ n ++ ": Not installed!\""
