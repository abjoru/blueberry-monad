module Main where

import           BMonad
import           BMonad.Bar
import           BMonad.Theme

import           XMonad
import           XMonad.Hooks.DynamicLog       (dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks      (docks, manageDocks)
import           XMonad.Layout.ShowWName
import           XMonad.Util.Hacks             (trayerPaddingXmobarEventHook)
import           XMonad.Util.NamedActions      (addDescrKeys')
import           XMonad.Util.Run               (hPutStrLn, spawnPipe)

import           System.Directory    (XdgDirectory (XdgConfig), getXdgDirectory)
import           System.FilePath     ((</>))

-----------------------------------------------------------
-- Theme Definition
-----------------------------------------------------------

bmonadTheme :: IO BMonadTheme
bmonadTheme = fmap f (getXdgDirectory XdgConfig "xmonad")
  where f d = BMonadTheme 
                { themeColors      = gruvboxDark 
                , themeFont        = "xft:SauceCodePro Nerd Font Mono:regular:size=11;antialias=true:hinting=true"
                , themeIconFolder  = (d </> "xpm")
                , themeBarAlpha    = 255
                , themeBorderWidth = 2
                }

-----------------------------------------------------------
-- Main entrypoint
-----------------------------------------------------------

main :: IO ()
main = do
  home    <- getHomeDirectory
  mobars  <- mkMobars
  apps    <- loadApplications
  theme   <- bMonadTheme
  layouts <- fmap myLayouts theme
  xdirs   <- getDirectories

  let myConfig = addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) (myKeys apps) $ ewmh $ docks $ def
                  { manageHook = myManageHook <+> manageDocks
                  , handleEventHook = trayerPaddingXmobarEventHook
                  , modMask = myModMask
                  , terminal = myTerminal
                  , startupHook = myStartupHook
                  , layoutHook = layouts
                  , workspaces = myWorkspaces
                  , borderWidth = myBorderWidth
                  , normalBorderColor = normColor theme
                  , focusedBorderColor = focusColor theme
                  , logHook = dynamicLogWithPP (bmobarPP theme windowCount mobars)
                  }

  launch myConfig xdirs
