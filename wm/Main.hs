module Main where

import           BMonad
import           BMonad.Bar
import           BMonad.Theme

import           XMonad
import           XMonad.Hooks.DynamicLog   (dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks  (docks, manageDocks)
import           XMonad.Util.Hacks         (trayerPaddingXmobarEventHook)
import           XMonad.Util.NamedActions  (addDescrKeys')

-----------------------------------------------------------
-- Main entrypoint
-----------------------------------------------------------

main :: IO ()
main = do
  mobars  <- mkMobars
  apps    <- loadApplications
  games   <- myGames
  theme   <- myTheme
  xdirs   <- getDirectories

  let myConfig = addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) (myKeys theme apps games) $ ewmh $ docks $ def
                  { manageHook = myManageHook <+> manageDocks
                  , handleEventHook = trayerPaddingXmobarEventHook
                  , modMask = myModMask
                  , terminal = myTerminal
                  , startupHook = myStartupHook
                  , layoutHook = myLayouts theme
                  , workspaces = myWorkspaces
                  , borderWidth = themeBorderWidth theme
                  , normalBorderColor = unfocusColor theme
                  , focusedBorderColor = focusColor theme
                  , logHook = dynamicLogWithPP (bmobarPP theme windowCount mobars)
                  }

  launch myConfig xdirs
