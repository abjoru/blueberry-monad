module Main where

-- Blueberry modules
import BMonad
import BMonad.Colors.GruvboxDark

-- Base
import XMonad

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (docks, manageDocks)
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.ShowWName (showWName')

-- Utilities
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.Hacks (trayerPaddingXmobarEventHook)
import XMonad.Util.NamedActions (addDescrKeys')

-- X11
import Graphics.X11.Xinerama (getScreenInfo)

-- GHC
import GHC.IO.Handle

-- System
import System.Directory
import System.FilePath ((</>))

--------------
-- Monitors --
--------------

-- Get X screen counts (i.e. # monitors)
countScreens :: IO Int
countScreens = do
  screens <- do
    dpy <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  pure $ length screens

-- Spawn xmobars for the given screen count
spawnBerrybars :: FilePath -> Int -> IO [Handle]
spawnBerrybars b 0 = (: []) <$> spawnPipe (b ++ " single") --"blueberry-mobar single"
spawnBerrybars b 1 = (: []) <$> spawnPipe (b ++ " single") --"blueberry-mobar single"
spawnBerrybars b 2 = mapM (mkBar b) [(0, "primary"), (1, "secondary")]
spawnBerrybars b 3 = mapM (mkBar b) [(0, "primary"), (1, "seondary"), (2, "other")]
spawnBerrybars b 4 = mapM (mkBar b) [(0, "primary"), (1, "other"), (2, "other"), (3, "secondary")]
spawnBerrybars b n = mapM (mkBar b) $ zip (take n [0..]) (["primary", "secondary"] ++ replicate (n - 2) "other")

mkBar :: FilePath -> (Int, String) -> IO Handle
mkBar b (i, m) = spawnPipe $ b ++ " -x " ++ show i ++ " " ++ m

xmobarOutput :: [Handle] -> String -> IO ()
xmobarOutput ha x = mapM_ (`hPutStrLn` x) ha

-----------
-- Mouse --
-----------


----------
-- MAIN --
----------
main :: IO ()
main = do
  home   <- getHomeDirectory
  mobars <- countScreens >>= spawnBerrybars (home </> ".local" </> "bin" </> "blueberry-mobar")
  apps   <- loadApplications
  xdirs  <- getDirectories

  let myConfig = addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) (myKeys apps) $ ewmh $ docks $ def
                  { manageHook = myManageHook <+> manageDocks
                  , handleEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "st-256color" <||> className =? "XTerm") (return True)
                                      <+> trayerPaddingXmobarEventHook
                  , modMask = myModMask
                  , terminal = myTerminal
                  , startupHook = myStartupHook
                  , layoutHook = showWName' myShowWNameTheme $ myLayoutHook
                  , workspaces = myWorkspaces
                  , borderWidth = myBorderWidth
                  , normalBorderColor = myNormColor
                  , focusedBorderColor = myFocusColor
                  , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                              { ppOutput = xmobarOutput mobars
                              , ppCurrent = xmobarColor color06 "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">") "</box>"
                              , ppVisible = xmobarColor color06 "" . clickable
                              , ppHidden = xmobarColor color05 "" . wrap ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">") "</box>"
                              , ppHiddenNoWindows = xmobarColor color05 "" . clickable
                              , ppTitle = xmobarColor color16 "" . shorten 60
                              , ppSep = "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
                              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
                              , ppExtras = [windowCount]
                              , ppOrder = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]
                              }
                  }

  launch myConfig xdirs
