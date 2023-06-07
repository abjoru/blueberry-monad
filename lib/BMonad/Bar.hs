module BMonad.Bar (Monitor(..), selectMonitor, bmonadBars, bmobarPP) where

import           BMonad.Bar.Utils        (fc)
import           BMonad.Bar.Widgets      (widgetCoins, widgetDate, widgetDisk,
                                          widgetFGI, widgetMem, widgetNet, widgetUpstream,
                                          widgetTrayerPadding, widgetXMonad)
import           BMonad.Config           
import           BMonad.Utils            (countScreens)

import Data.Maybe (mapMaybe)
import Data.List (intercalate)

import           System.Directory        (getHomeDirectory)
import           System.FilePath         ((</>))

import           GHC.IO.Handle           (Handle)

import qualified Xmobar                  as M

import           XMonad                  (X)
import           XMonad.Hooks.DynamicLog (PP (..), shorten, wrap, xmobarColor,
                                          xmobarPP)
import           XMonad.Util.Run         (hPutStrLn, spawnPipe)

data Monitor = Primary | Secondary | Other

icon :: String
icon = " <icon=haskell_20.xpm/>"

defaultHeight :: Int
defaultHeight = 24

baseConfig :: Config -> M.Config
baseConfig cfg         = M.defaultConfig
  { M.font             = msFont $ cfgMobarSettings cfg
  , M.borderColor      = mobarBorderColor cfg
  , M.fgColor          = mobarFgColor cfg
  , M.bgColor          = mobarBgColor cfg
  , M.border           = msBorder $ cfgMobarSettings cfg
  , M.alpha            = mobarAlpha cfg
  , M.overrideRedirect = msOverrideRedirect $ cfgMobarSettings cfg
  , M.lowerOnStart     = msLowerOnStartup $ cfgMobarSettings cfg
  , M.hideOnStart      = msHideOnStartup $ cfgMobarSettings cfg
  , M.allDesktops      = msAllDesktops $ cfgMobarSettings cfg
  , M.persistent       = msPersistent $ cfgMobarSettings cfg
  , M.sepChar          = "%"
  , M.alignSep         = "}{"
  , M.iconRoot         = cfgMonadDir cfg </> "xpm"
  , M.position         = M.TopSize M.C 100 defaultHeight
  , M.additionalFonts  = [ "Cryptocurrency 14" -- fn=1
                         , "Font Awesome 6 Free, DroidSansMono Nerd Font Mono 10, 10" -- fn=2
                         , "Font Awesome 6 Brands 10" -- fn=3
                         , "Hasklug Nerd Font Mono 10" -- fn=4
                         ]
  }

mkConfig :: Config -> [M.Runnable] -> String -> M.Config
mkConfig cfg cmds tmpl = (baseConfig cfg) { M.commands = cmds, M.template = tmpl }

spawnBars :: FilePath -> Int -> IO [Handle]
spawnBars b 0 = (: []) <$> spawnPipe (b ++ " single")
spawnBars b 1 = (: []) <$> spawnPipe (b ++ " single")
spawnBars b 2 = mapM (mkBar b) [(0, "primary"), (1, "secondary")]
spawnBars b 3 = mapM (mkBar b) [(0, "primary"), (1, "secondary"), (2, "other")]
spawnBars b 4 = mapM (mkBar b) [(0, "primary"), (1, "other"), (2, "other"), (3, "secondary")]
spawnBars b n = mapM (mkBar b) $ zip (take n [0..]) (["primary", "secondary"] ++ replicate (n - 2) "other")

mkBar :: FilePath -> (Int, String) -> IO Handle
mkBar b (i, m) = spawnPipe $ b ++ " -x " ++ show i ++ " " ++ m

bmonadBars :: IO [Handle]
bmonadBars = do
  home <- getHomeDirectory
  countScreens >>= spawnBars (home </> ".local" </> "bin" </> "blueberry-mobar")

bmobarPP :: Config -> X (Maybe String) -> [Handle] -> PP
bmobarPP cfg wcount bars =
  let cCur   = color04 . themeColorScheme $ cfgTheme cfg
      cVis   = color04 . themeColorScheme $ cfgTheme cfg
      cHid   = color05 . themeColorScheme $ cfgTheme cfg
      cHidN  = color05 . themeColorScheme $ cfgTheme cfg
      cSep   = color09 . themeColorScheme $ cfgTheme cfg
      cTitle = color16 . themeColorScheme $ cfgTheme cfg
      cUrg   = color02 . themeColorScheme $ cfgTheme cfg
   in xmobarPP
      { ppOutput = outputBars bars
      , ppCurrent = xmobarColor cCur "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ cCur ++ ">") "</box>"
      , ppVisible = xmobarColor cVis ""
      , ppHidden = xmobarColor cHid "" . wrap ("<box type=Top width=2 mb=2 color=" ++ cHid ++ ">") "</box>"
      , ppHiddenNoWindows = xmobarColor cHidN ""
      , ppTitle = xmobarColor cTitle "" . shorten 60
      , ppSep = "<fc=" ++ cSep ++ "> <fn=1>|</fn> </fc>"
      , ppUrgent = xmobarColor cUrg "" . wrap "!" "!"
      , ppExtras = [wcount]
      , ppOrder = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]
      }
  where outputBars hs s = mapM_ (`hPutStrLn` s) hs

{-----------------------------------------------------
  Monitor Settings
-----------------------------------------------------}

selectMonitor :: Config -> Maybe Monitor -> M.Config
selectMonitor cfg (Just Primary)   = primaryMonitor cfg
selectMonitor cfg (Just Secondary) = secondaryMonitor cfg
selectMonitor cfg (Just Other)     = otherMonitor cfg
selectMonitor cfg Nothing          = singleMonitor cfg

mkWidget :: Scheme -> MobarWidget -> Maybe M.Runnable
mkWidget s CoinPriceWidget = Just $ widgetCoins s
mkWidget s FearGreedWidget = Just $ widgetFGI s
mkWidget s MemoryWidget    = Just $ widgetMem s
mkWidget s DiskUWidget     = Just $ widgetDisk s
mkWidget s NetworkWidget   = Just $ widgetNet s
mkWidget s DateWidget      = Just $ widgetDate s
mkWidget s UpstreamWidget  = Just $ widgetUpstream s
mkWidget _ TrayWidget      = Just widgetTrayerPadding
mkWidget _ _               = Nothing

mkTemplate :: MobarWidget -> Maybe String
mkTemplate CoinPriceWidget = Just "%coinprices%"
mkTemplate FearGreedWidget = Just "%fgi%"
mkTemplate MemoryWidget    = Just "%memory%"
mkTemplate DiskUWidget     = Just "%disku%"
mkTemplate NetworkWidget   = Just "%dynnetwork%"
mkTemplate DateWidget      = Just "%date%"
mkTemplate UpstreamWidget  = Just "%upstream%"
mkTemplate TrayWidget      = Just "%_XMONAD_TRAYPAD%"
mkTemplate _               = Nothing

mkMonitorBar :: Config -> (Config -> [MobarWidget]) -> M.Config
mkMonitorBar cfg func =
  let scheme  = themeColorScheme $ cfgTheme cfg
      widgets = func cfg
      cmds    = widgetXMonad : mapMaybe (mkWidget scheme) widgets
      sep     = fc (color09 scheme) " | "
      barStrs = mapMaybe mkTemplate widgets
      templ   = intercalate sep barStrs
   in mkConfig cfg cmds (icon ++ sep ++ "%UnsafeStdinReader% }{ " ++ templ)

singleMonitor :: Config -> M.Config
singleMonitor cfg = mkMonitorBar cfg mobarSingleMonitor

primaryMonitor :: Config -> M.Config
primaryMonitor cfg = mkMonitorBar cfg mobarPrimaryMonitor

secondaryMonitor :: Config -> M.Config
secondaryMonitor cfg = mkMonitorBar cfg mobarSecondaryMonitor

otherMonitor :: Config -> M.Config
otherMonitor cfg = mkMonitorBar cfg mobarOtherMonitors
