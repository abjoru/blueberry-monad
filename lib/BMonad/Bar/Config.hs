module BMonad.Bar.Config (selectMonitor, Monitor(..)) where

import           Xmobar

import           BMonad.Bar.Utils
import           BMonad.Bar.Widgets
import           BMonad.Theme       hiding (borderColor)
import           BMonad.Variables   (myXMonadDir)

import           System.FilePath    ((</>))

data Monitor = Primary | Secondary | Other

icon = " <icon=haskell_20.xpm/>"

defaultHeight :: Int
defaultHeight = 24

baseConfig :: BMonadTheme -> Config
baseConfig t = defaultConfig
  { font = themeFont t
  , borderColor = color08 $ themeColors t
  , fgColor = colorFore $ themeColors t
  , bgColor = colorBack $ themeColors t
  , border = BottomB
  , alpha = themeBarAlpha t
  , overrideRedirect = True
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , sepChar = "%"
  , alignSep = "}{"
  , iconRoot = themeIconFolder t
  , position = TopSize C 100 defaultHeight
  , textOffset = defaultHeight - 8
  , textOffsets = [defaultHeight - 9]
  , additionalFonts = [ "xft:Symbola-9"
                      , "xft:Symbola-10"
                      , "xft:Symbola-11"
                      , "xft:Symbola-12"
                      , "xft:Hack-7"
                      , "xft:FontAwesome-9"
                      ]
  }

mkConfig :: BMonadTheme -> [Runnable] -> String -> Config
mkConfig t cs tmpl = (baseConfig t) { commands = cs, template = tmpl }

---------------------
-- Monitor Configs --
---------------------

selectMonitor :: BMonadTheme -> Maybe Monitor -> IO Config
selectMonitor t (Just Primary)   = primaryMonitor t
selectMonitor t (Just Secondary) = secondaryMonitor t
selectMonitor t (Just Other)     = otherMonitor t
selectMonitor t Nothing          = singleMonitor t

singleMonitor :: BMonadTheme -> IO Config
singleMonitor t = do
  wup <- widgetUpdates t

  let (<|>) a b = a ++ fc (sepColor t) " | " ++ b
      cmds      = [wup, widgetTrayerPadding, widgetXmonad, widgetDate t, widgetMem t, widgetNet t, widgetDisk t, widgetWeather t]
      tmpl      = icon
                  <|> "%UnsafeStdinReader% }{ %KFXE%"
                  <|> "%memory%"
                  <|> "%disku%"
                  <|> "%dynnetwork%"
                  <|> "%updates%"
                  <|> "%date%"
                  <|> "%_XMONAD_TRAYPAD%"

  return $ mkConfig t cmds tmpl

primaryMonitor :: BMonadTheme -> IO Config
primaryMonitor t = do
  wup <- widgetUpdates t

  let (<|>) a b = a ++ fc (sepColor t) " | " ++ b
      cmds      = [wup, widgetTrayerPadding, widgetXmonad, widgetDate t, widgetNet t]
      tmpl      = icon
                  <|> "%UnsafeStdinReader% }{ %dynnetwork%"
                  <|> "%updates%"
                  <|> "%date%"
                  <|> "%_XMONAD_TRAYPAD%"

  return $ mkConfig t cmds tmpl

secondaryMonitor :: BMonadTheme -> IO Config
secondaryMonitor t = do
  let (<|>) a b = a ++ fc (sepColor t) " | " ++ b
      cmds      = [widgetXmonad, widgetCoins t, widgetFearGreed t, widgetDate t, widgetMem t, widgetDisk t]
      tmpl      = icon 
                  <|> "%UnsafeStdinReader% }{ %coinprice%" 
                  <|> "%fg%" 
                  <|> "%memory%" 
                  <|> "%disku%" 
                  <|> "%date% "

  return $ mkConfig t cmds tmpl

otherMonitor :: BMonadTheme -> IO Config
otherMonitor t = do
  let (<|>) a b = a ++ fc (sepColor t) " | " ++ b
      cmds      = [widgetXmonad, widgetDate t, widgetWeather t]
      tmpl      = icon
                  <|> "%UnsafeStdinReader% }{ %KFXE%"
                  <|> "%date% "

  return $ mkConfig t cmds tmpl
