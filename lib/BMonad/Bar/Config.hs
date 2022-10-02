module BMonad.Bar.Config (selectMonitor, Monitor(..)) where

import           Xmobar

import           BMonad.Bar.Utils
import BMonad.Bar.Widgets
import           BMonad.Colors
import BMonad.Variables (myFont, myXMonadDir)

import System.FilePath ((</>))

data Monitor = Primary | Secondary | Other

defaultHeight :: Int
defaultHeight = 24

sep :: Colors -> String -> String -> String
sep c f s = f ++ fcBg c " | " ++ s

baseConfig :: XMobarTheme -> Config
baseConfig t = defaultConfig 
  { font = mobarFont t
  , borderColor = mobarBorder t
  , fgColor = mobarFore t
  , bgColor = mobarBack t
  , border = BottomB
  , alpha = mobarAlpha t
  , overrideRedirect = True
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , sepChar = "%"
  , alignSep = "}{"
  , iconRoot = mobarIconRoot t
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

mkConfig :: XMobarTheme -> [Runnable] -> String -> Config
mkConfig t cs tmpl = (baseConfig t) { commands = cs, template = tmpl }

---------------------
-- Monitor Configs --
---------------------

selectMonitor :: XMobarTheme -> Maybe Monitor -> IO Config
selectMonitor t (Just Primary)   = primaryMonitor t
selectMonitor t (Just Secondary) = secondaryMonitor t
selectMonitor t (Just Other)     = otherMonitor t
selectMonitor t Nothing          = singleMonitor t

singleMonitor :: XMobarTheme -> IO Config
singleMonitor t = do
  wup <- widgetUpdates $ mobarColors t
  
  let c         = mobarColors t
      (<|>) a b = a ++ fcBg c " | " ++ b
      ws        = [wup, widgetTrayerPadding, widgetXmonad, widgetDate c, widgetMem c, widgetNet c, widgetDisk c, widgetWeather c]
      cmds      = [wup, widgetTrayerPadding, widgetXmonad, widgetDate c, widgetMem c, widgetNet c, widgetDisk c, widgetWeather c]
      tmpl      = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %KFXE%" <|> "%memory%" <|> "%disku%" <|> "%dynnetwork%" <|> "%updates%" <|> "%date%" <|> "%_XMONAD_TRAYPAD%"

  return $ mkConfig t cmds tmpl

primaryMonitor :: XMobarTheme -> IO Config
primaryMonitor t = do
  wup <- widgetUpdates $ mobarColors t

  let c         = mobarColors t
      (<|>) a b = a ++ fcBg c " | " ++ b
      cmds      = [wup, widgetTrayerPadding, widgetXmonad, widgetDate c, widgetNet c]
      tmpl      = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %dynnetwork%" <|> "%updates%" <|> "%date%" <|> "%_XMONAD_TRAYPAD%"

  return $ mkConfig t cmds tmpl

secondaryMonitor :: XMobarTheme -> IO Config
secondaryMonitor t = do
  let c         = mobarColors t
      (<|>) a b = a ++ fcBg c " | " ++ b
      cmds      = [widgetXmonad, widgetCoins c, widgetFearGreed c, widgetDate c, widgetMem c, widgetDisk c]
      tmpl      = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %coinprice%" <|> "%fg%" <|> "%memory%" <|> "%disku%" <|> "%date% "

  return $ mkConfig t cmds tmpl

otherMonitor :: XMobarTheme -> IO Config
otherMonitor t = do
  let c         = mobarColors t
      (<|>) a b = a ++ fcBg c " | " ++ b
      cmds      = [widgetXmonad, widgetDate c, widgetWeather c]
      tmpl      = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %KFXE%" <|> "%date% "

  return $ mkConfig t cmds tmpl
