{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module BMonad.Config.Types (
  Font,
  Color,
  Scheme(..),
  Theme(..),
  Config(..),
  GridSettings(..),
  MobarWidget(..),
  MobarSettings(..),
  App(..),
  Category(..),

  ReadTheme(..),
  ReadConfig(..),

  mobarBorderColor,
  mobarFgColor,
  mobarBgColor,
  mobarAlpha,
  mobarSingleMonitor,
  mobarPrimaryMonitor,
  mobarSecondaryMonitor,
  mobarOtherMonitors,
  monadWindowSpacing,

  defaultTheme,
  defaultConfig,
  findSchemeWithDefault
) where

import qualified Data.HashMap.Strict as HM
import           Data.Word           (Word32)
import           Data.Yaml

import           System.Directory    (XdgDirectory (XdgConfig),
                                      getHomeDirectory, getXdgDirectory)
import           System.FilePath     ((</>))
import           System.Log          (Priority (..))

import           Xmobar              (Border (BottomB, FullB, NoBorder, TopB))
import           XMonad              (Dimension, KeyMask, mod1Mask, mod4Mask)
import Data.Maybe (fromMaybe)

type Font = String

type Color = String

data Scheme = Scheme
  { colorBack :: Color
  , colorFore :: Color
  , color01   :: Color
  , color02   :: Color
  , color03   :: Color
  , color04   :: Color
  , color05   :: Color
  , color06   :: Color
  , color07   :: Color
  , color08   :: Color
  , color09   :: Color
  , color10   :: Color
  , color11   :: Color
  , color12   :: Color
  , color13   :: Color
  , color14   :: Color
  , color15   :: Color
  , color16   :: Color
  } deriving (Show, Eq)

data Theme = Theme
  { themeColorScheme :: Scheme
  , themeFont        :: Font
  , themeBarAlpha    :: Int
  , themeBorderWidth :: Dimension
  } deriving (Show, Eq)

data GridSettings = GridSettings
  { gsCellHeight   :: Integer
  , gsCellWidth    :: Integer
  , gsCellPadding  :: Integer
  , gsOriginFractX :: Double
  , gsOriginFractY :: Double
  } deriving (Show, Eq)

data MobarWidget
  = CoinPriceWidget
  | FearGreedWidget
  | MemoryWidget
  | DiskUWidget
  | NetworkWidget
  | DateWidget
  | UpstreamWidget
  | TrayWidget
  | UndefinedWidget
  deriving (Show, Eq)

data MobarSettings = MobarSettings
  { msFont             :: Font
  , msBorderColor      :: Maybe Color
  , msFgColor          :: Maybe Color
  , msBgColor          :: Maybe Color
  , msBorder           :: Border
  , msAlpha            :: Maybe Int
  , msOverrideRedirect :: Bool
  , msLowerOnStartup   :: Bool
  , msHideOnStartup    :: Bool
  , msAllDesktops      :: Bool
  , msPersistent       :: Bool
  , msSingleMonitor    :: [MobarWidget]
  , msPrimaryMonitor   :: [MobarWidget]
  , msSecondaryMonitor :: [MobarWidget]
  , msOtherMonitors    :: [MobarWidget]
  } deriving (Show, Eq)

data Config = Config
  { cfgTheme         :: Theme
  , cfgMonadDir      :: FilePath
  , cfgGameDir       :: FilePath
  , cfgModMask       :: KeyMask
  , cfgAltMask       :: KeyMask
  , cfgTerminal      :: String
  , cfgBrowser       :: String
  , cfgEditor        :: String
  , cfgSoundPlayer   :: String
  , cfgWorkspaces    :: [String]
  , cfgWindowSpacing :: Maybe Integer
  , cfgLogLevel      :: Priority
  , cfgGridSelect    :: GridSettings
  , cfgMobarSettings :: MobarSettings
  } deriving (Show, Eq)

data App = App
  { appName     :: String
  , appDesc     :: String
  , appLauncher :: Maybe String
  , appCommand  :: Maybe String
  } deriving (Show, Eq)

data Category = Category
  { categoryName :: String
  , categoryApps :: [App]
  } deriving (Show, Eq)

{-------------------------------------------
  Reader types
-------------------------------------------}

data ReadTheme = ReadTheme
  { rtName        :: String
  , rtFont        :: String
  , rtAlpha       :: Int
  , rtBorderWidth :: Word32
  } deriving (Show, Eq)

data ReadConfig = ReadConfig
  { rcLogLevel      :: Priority
  , rcTheme         :: ReadTheme
  , rcGameDir       :: String
  , rcTerminal      :: String
  , rcBrowser       :: String
  , rcEditor        :: String
  , rcSound         :: String
  , rcWorkspaces    :: [String]
  , rcWindowSpacing :: Maybe Integer
  , rcGridSelect    :: GridSettings
  , rcMobarSettings :: MobarSettings
  } deriving (Show, Eq)

{-------------------------------------------
  Instances
-------------------------------------------}

instance FromJSON Priority where
  parseJSON (String "DEBUG") = return DEBUG
  parseJSON (String "INFO")  = return INFO
  parseJSON (String "ERROR") = return ERROR
  parseJSON _                = return WARNING

instance FromJSON Border where
  parseJSON (String "TopB")    = return TopB
  parseJSON (String "BottomB") = return BottomB
  parseJSON (String "FullB")   = return FullB
  parseJSON _                  = return NoBorder

instance FromJSON MobarWidget where
  parseJSON (String "CoinPrice") = return CoinPriceWidget
  parseJSON (String "FearGreed") = return FearGreedWidget
  parseJSON (String "Memory")    = return MemoryWidget
  parseJSON (String "DiskU")     = return DiskUWidget
  parseJSON (String "Network")   = return NetworkWidget
  parseJSON (String "Date")      = return DateWidget
  parseJSON (String "Upstream")  = return UpstreamWidget
  parseJSON (String "Tray")      = return TrayWidget
  parseJSON _                    = return UndefinedWidget

instance FromJSON ReadTheme where
  parseJSON = withObject "Theme" $ \v -> ReadTheme
    <$> v .:? "name" .!= defaultThemeName
    <*> v .:? "font" .!= defaultFont
    <*> v .:? "alpha" .!= defaultAlpha
    <*> v .:? "border-width" .!= defaultBorderWidth

instance FromJSON ReadConfig where
  parseJSON = withObject "Config" $ \v -> ReadConfig
    <$> v .:? "log-level" .!= defaultLogLevel
    <*> v .:? "theme" .!= defaultReadTheme
    <*> v .:? "game-folder" .!= defaultGameDir
    <*> v .:? "terminal" .!= defaultTerminal
    <*> v .:? "browser" .!= defaultBrowser
    <*> v .:? "editor" .!= defaultEditor
    <*> v .:? "sound-player" .!= defaultSoundPlayer
    <*> v .:? "workspaces" .!= defaultWorkspaces
    <*> v .:? "window-spacing" .!= Just defaultWindowSpacing
    <*> v .:? "grid-select" .!= defaultGridSettings
    <*> v .:? "mobar" .!= defaultMobarSettings

instance FromJSON Scheme where
  parseJSON = withObject "Scheme" $ \v -> Scheme
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

instance FromJSON GridSettings where
  parseJSON = withObject "GridSelect" $ \v -> GridSettings
    <$> v .:? "cellheight" .!= 40
    <*> v .:? "cellwidth" .!= 250
    <*> v .:? "cellpadding" .!= 6
    <*> v .:? "origin-fract-x" .!= 0.5
    <*> v .:? "origin-fract-y" .!= 0.5

instance FromJSON MobarSettings where
  parseJSON = withObject "Mobar" $ \v -> MobarSettings
    <$> v .:? "font" .!= defaultMobarFont
    <*> v .:? "border-color"
    <*> v .:? "fg-color"
    <*> v .:? "bg-color"
    <*> v .:? "border" .!= BottomB
    <*> v .:? "alpha"
    <*> v .:? "override-redirect" .!= True
    <*> v .:? "lower-on-startup" .!= True
    <*> v .:? "hide-on-startup" .!= False
    <*> v .:? "all-desktops" .!= False
    <*> v .:? "persistent" .!= True
    <*> v .:? "single-monitor" .!= [MemoryWidget, DiskUWidget, NetworkWidget, DateWidget, TrayWidget]
    <*> v .:? "primary-monitor" .!= [NetworkWidget, DateWidget, TrayWidget]
    <*> v .:? "secondary-monitor" .!= [MemoryWidget, DiskUWidget, DateWidget]
    <*> v .:? "other-monitors" .!= [DateWidget]

{-------------------------------------------
  Accessors
-------------------------------------------}

mobarBorderColor :: Config -> Color
mobarBorderColor cfg = findC $ cfgMobarSettings cfg
  where findC (MobarSettings _ (Just c) _ _ _ _ _ _ _ _ _ _ _ _ _) = c
        findC _ = color08 . themeColorScheme $ cfgTheme cfg

mobarFgColor :: Config -> Color
mobarFgColor cfg = findC $ cfgMobarSettings cfg
  where findC (MobarSettings _ _ (Just c) _ _ _ _ _ _ _ _ _ _ _ _) = c
        findC _ = colorFore . themeColorScheme $ cfgTheme cfg

mobarBgColor :: Config -> Color
mobarBgColor cfg = findC $ cfgMobarSettings cfg
  where findC (MobarSettings _ _ _ (Just c) _ _ _ _ _ _ _ _ _ _ _) = c
        findC _ = colorBack . themeColorScheme $ cfgTheme cfg

mobarAlpha :: Config -> Int
mobarAlpha cfg = findA $ cfgMobarSettings cfg
  where findA (MobarSettings _ _ _ _ _ (Just a) _ _ _ _ _ _ _ _ _) = a
        findA _ = themeBarAlpha $ cfgTheme cfg

mobarSingleMonitor :: Config -> [MobarWidget]
mobarSingleMonitor = msSingleMonitor . cfgMobarSettings

mobarPrimaryMonitor :: Config -> [MobarWidget]
mobarPrimaryMonitor = msPrimaryMonitor . cfgMobarSettings

mobarSecondaryMonitor :: Config -> [MobarWidget]
mobarSecondaryMonitor = msSecondaryMonitor . cfgMobarSettings

mobarOtherMonitors :: Config -> [MobarWidget]
mobarOtherMonitors = msOtherMonitors . cfgMobarSettings

monadWindowSpacing :: Config -> Integer
monadWindowSpacing cfg = fromMaybe defaultWindowSpacing $ cfgWindowSpacing cfg

{-------------------------------------------
  Defaults
-------------------------------------------}

defaultThemeName :: String
defaultThemeName = "gruvboxDark"

defaultFont :: Font
defaultFont = "xft:Mononoki Nerd Font:pixelsize=11:antialias=true:hinting=true"

defaultMobarFont :: Font
defaultMobarFont = "Mononoki Nerd Font 7"

defaultAlpha :: Int
defaultAlpha = 255

defaultBorderWidth :: Dimension
defaultBorderWidth = 2

defaultLogLevel :: Priority
defaultLogLevel = WARNING

defaultReadTheme :: ReadTheme
defaultReadTheme = ReadTheme defaultThemeName defaultFont defaultAlpha defaultBorderWidth

defaultGameDir :: String
defaultGameDir = "~/Games"

defaultTerminal :: String
defaultTerminal = "alacritty"

defaultBrowser :: String
defaultBrowser = "qutebrowser"

defaultEditor :: String
defaultEditor = defaultTerminal ++ " -e nvim "

defaultSoundPlayer :: String
defaultSoundPlayer = "ffplay -nodisp --autoexit"

defaultWorkspaces :: [String]
defaultWorkspaces = [" alpha ", " bravo ", " charlie ", " delta "]

defaultWindowSpacing :: Integer
defaultWindowSpacing = 4

defaultScheme :: Scheme
defaultScheme =
    Scheme
        { colorBack = "#282828"
        , colorFore = "#ebdbb2"
        , color01   = "#282828"
        , color02   = "#cc241d"
        , color03   = "#98971a"
        , color04   = "#d79921"
        , color05   = "#458588"
        , color06   = "#b16286"
        , color07   = "#689d6a"
        , color08   = "#a89984"
        , color09   = "#928374"
        , color10   = "#fb4934"
        , color11   = "#b8bb26"
        , color12   = "#fabd2f"
        , color13   = "#83a598"
        , color14   = "#d3869b"
        , color15   = "#8ec07c"
        , color16   = "#ebdbb2"
        }

defaultTheme :: Theme
defaultTheme = Theme defaultScheme defaultFont defaultAlpha defaultBorderWidth

defaultConfig :: IO Config
defaultConfig = do
  hdir <- getHomeDirectory
  bdir <- getXdgDirectory XdgConfig "xmonad"
  return $ Config { cfgTheme         = defaultTheme
                  , cfgMonadDir      = bdir
                  , cfgGameDir       = hdir </> "Games"
                  , cfgModMask       = mod4Mask
                  , cfgAltMask       = mod1Mask
                  , cfgTerminal      = defaultTerminal
                  , cfgBrowser       = defaultBrowser
                  , cfgEditor        = defaultEditor
                  , cfgSoundPlayer   = defaultSoundPlayer
                  , cfgWorkspaces    = defaultWorkspaces
                  , cfgWindowSpacing = Just defaultWindowSpacing
                  , cfgLogLevel      = defaultLogLevel
                  , cfgGridSelect    = defaultGridSettings
                  , cfgMobarSettings = defaultMobarSettings
                  }

defaultGridSettings :: GridSettings
defaultGridSettings = GridSettings 40 250 6 0.5 0.5

defaultMobarSettings :: MobarSettings
defaultMobarSettings = MobarSettings
  { msFont             = defaultMobarFont
  , msBorderColor      = Nothing
  , msFgColor          = Nothing
  , msBgColor          = Nothing
  , msBorder           = BottomB
  , msAlpha            = Nothing
  , msOverrideRedirect = True
  , msLowerOnStartup   = True
  , msHideOnStartup    = False
  , msAllDesktops      = True
  , msPersistent       = True
  , msSingleMonitor    = [MemoryWidget, DiskUWidget, NetworkWidget, DateWidget, TrayWidget]
  , msPrimaryMonitor   = [NetworkWidget, DateWidget, TrayWidget]
  , msSecondaryMonitor = [MemoryWidget, DiskUWidget, DateWidget]
  , msOtherMonitors    = [DateWidget]
  }

{-------------------------------------------
  Helpers
-------------------------------------------}

findSchemeWithDefault :: String -> [(String, Scheme)] -> Scheme
findSchemeWithDefault name arr = HM.findWithDefault defaultScheme name (HM.fromList arr)
