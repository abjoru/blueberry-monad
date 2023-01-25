module BMonad.Bar (Monitor(..), selectMonitor, bmonadBars, bmobarPP) where

import           BMonad.Bar.Utils        (fc)
import           BMonad.Bar.Widgets      (widgetCoins, widgetDate, widgetDisk,
                                          widgetFGI, widgetMem, widgetNet,
                                          widgetTrayerPadding, widgetXMonad)
import           BMonad.Config           (Config (cfgMobarSettings, cfgMonadDir, cfgTheme),
                                          MobarSettings (msAllDesktops, msBorder, msHideOnStartup, msLowerOnStartup, msOverrideRedirect, msPersistent),
                                          Scheme (color02, color04, color05, color09, color16),
                                          Theme (themeColorScheme), mobarAlpha,
                                          mobarBgColor, mobarBorderColor,
                                          mobarFgColor, mobarFont)
import           BMonad.Utils            (countScreens)

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
  { M.font             = mobarFont cfg
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
  , M.textOffset       = defaultHeight - 8
  , M.textOffsets      = [defaultHeight - 9]
  }

mkConfig :: Config -> [M.Runnable] -> String -> M.Config
mkConfig cfg cmds tmpl = (baseConfig cfg) { M.commands = cmds, M.template = tmpl }

spawnBars :: FilePath -> Int -> IO [Handle]
spawnBars b 0 = (: []) <$> spawnPipe (b ++ " single")
spawnBars b 1 = (: []) <$> spawnPipe (b ++ " single")
spawnBars b 2 = mapM (mkBar b) [(0, "primary"), (1, "secondary")]
spawnBars b 3 = mapM (mkBar b) [(0, "primary"), (1, "secondary"), (2, "other")]
spawnBars b 4 = mapM (mkBar b) [(0, "primary"), (1, "secondary"), (2, "other"), (3, "secondary")]
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

singleMonitor :: Config -> M.Config
singleMonitor cfg =
  let scheme    = themeColorScheme $ cfgTheme cfg
      (<|>) a b = a ++ fc (color09 scheme) " | " ++ b
      cmds      = [ widgetTrayerPadding
                  , widgetXMonad
                  , widgetCoins scheme
                  , widgetFGI scheme
                  , widgetDate scheme
                  , widgetMem scheme
                  , widgetNet scheme
                  , widgetDisk scheme
                  ]
      tmpl      = icon
                  <|> "%UnsafeStdinReader% }{ %coinprice%"
                  <|> "%fgi%"
                  <|> "%memory%"
                  <|> "%disku%"
                  <|> "%dynnetwork%"
                  <|> "%date%"
                  <|> "%_XMONAD_TRAYPAD%"
   in mkConfig cfg cmds tmpl

primaryMonitor :: Config -> M.Config
primaryMonitor cfg =
  let scheme    = themeColorScheme $ cfgTheme cfg
      (<|>) a b = a ++ fc (color09 scheme) " | " ++ b
      cmds      = [ widgetTrayerPadding
                  , widgetXMonad
                  , widgetDate scheme
                  , widgetNet scheme
                  ]
      tmpl      = icon
                  <|> "%UnsafeStdinReader% }{ %dynnetwork%"
                  <|> "%date%"
                  <|> "%_XMONAD_TRAYPAD%"
   in mkConfig cfg cmds tmpl

secondaryMonitor :: Config -> M.Config
secondaryMonitor cfg =
  let scheme    = themeColorScheme $ cfgTheme cfg
      (<|>) a b = a ++ fc (color09 scheme) " | " ++ b
      cmds      = [ widgetXMonad
                  , widgetCoins scheme
                  , widgetFGI scheme
                  , widgetDate scheme
                  , widgetMem scheme
                  , widgetDisk scheme
                  ]
      tmpl      = icon
                  <|> "%UnsafeStdinReader% }{ %coinprice%"
                  <|> "%fgi%"
                  <|> "%memory%"
                  <|> "%disku%"
                  <|> "%date%"
   in mkConfig cfg cmds tmpl

otherMonitor :: Config -> M.Config
otherMonitor cfg =
  let scheme    = themeColorScheme $ cfgTheme cfg
      (<|>) a b = a ++ fc (color09 scheme) " | " ++ b
      cmds      = [ widgetXMonad, widgetDate scheme ]
      tmpl      = icon <|> "%UnsafeStdinReader% }{ %date%"
   in mkConfig cfg cmds tmpl
