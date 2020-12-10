module Blueberry.BarConfig (selectMonitor, Monitor(..)) where

import Blueberry.Utils
import Blueberry.Palette
import Blueberry.BarWidgets

import Xmobar

data Monitor = Primary | Secondary | Other

defaultHeight :: Int
defaultHeight = 24

(<|>) :: String -> String -> String
(<|>) f s = f ++ (fcBg2 " | ") ++ s

baseConfig :: MobarPalette -> Config
baseConfig p = defaultConfig 
  { font = pFont p
  , borderColor = pBorder p
  , fgColor = pForeground p
  , bgColor = pBackground p
  , border = BottomB
  , alpha = pAlpha p
  , overrideRedirect = True
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , sepChar = "%"
  , alignSep = "}{"
  , iconRoot = pIconRoot p
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

mkConfig :: MobarPalette -> [Runnable] -> String -> Config
mkConfig p cmd templ = (baseConfig p) { commands = cmd, template = templ }

---------------------
-- Monitor Configs --
---------------------

selectMonitor :: MobarPalette -> Maybe Monitor -> IO Config
selectMonitor p (Just Primary)   = primaryMonitor p
selectMonitor p (Just Secondary) = secondaryMonitor p
selectMonitor p (Just Other)     = otherMonitor p
selectMonitor p Nothing          = singleMonitor p

singleMonitor :: MobarPalette -> IO Config
singleMonitor p = do
  ccp <- cmdCpunk
  cup <- cmdUpd
  ctr <- cmdTray

  let cmds = [ccp, cup, ctr, cmdXmonad, cmdDate, cmdMem p, cmdNet p, cmdDisk p, cmdWeather p]
      tmpl = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %cpunk%" <|> "%KFXE%" <|> "%memory%" <|> "%disku%" <|> "%dynnetwork%" <|> "%updates%" <|> "%date%" <|> "%trayer%"

  return $ mkConfig p cmds tmpl

primaryMonitor :: MobarPalette -> IO Config
primaryMonitor p = do
  ccp <- cmdCpunk
  cup <- cmdUpd
  ctr <- cmdTray

  let cmds = [ccp, cup, ctr, cmdXmonad, cmdDate, cmdNet p]
      tmpl = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %cpunk%" <|> "%dynnetwork%" <|> "%updates%" <|> "%date%" <|> "%trayer%"

  return $ mkConfig p cmds tmpl


secondaryMonitor :: MobarPalette -> IO Config
secondaryMonitor p = do
  ccp <- cmdCpunk

  let cmds = [ccp, cmdXmonad, cmdDate, cmdMem p, cmdDisk p]
      tmpl = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %cpunk%" <|> "%memory%" <|> "%disku%" <|> "%date% "

  return $ mkConfig p cmds tmpl

otherMonitor :: MobarPalette -> IO Config
otherMonitor p = do
  ccp <- cmdCpunk
  let cmds = [ccp, cmdXmonad, cmdDate, cmdWeather p]
      tmpl = " <icon=haskell_20.xpm/>" <|> "%UnsafeStdinReader% }{ %cpunk%" <|> "%KFXE%" <|> "%date% "

  return $ mkConfig p cmds tmpl
