module Main where

import           BMonad
import           BMonad.Bar
import           BMonad.Theme

-- Base
import           XMonad

import           XMonad.Hooks.DynamicLog       (dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks      (docks, manageDocks)
import           XMonad.Hooks.WindowSwallowing

import           XMonad.Layout.ShowWName

-- Utilities
import           XMonad.Util.Hacks             (trayerPaddingXmobarEventHook)
import           XMonad.Util.NamedActions      (addDescrKeys')
import           XMonad.Util.Run               (hPutStrLn, spawnPipe)

-- X11
import           Graphics.X11.Xinerama         (getScreenInfo)

-- GHC
import           GHC.IO.Handle

-- System
import           System.Directory
import           System.FilePath               ((</>))

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

----------
-- MAIN --
----------
main :: IO ()
main = do
  home    <- getHomeDirectory
  mobars  <- countScreens >>= spawnBerrybars (home </> ".local" </> "bin" </> "blueberry-mobar")
  apps    <- loadApplications
  theme   <- myTheme
  layouts <- myLayoutHook
  xdirs   <- getDirectories

  let myConfig = addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) (myKeys apps) $ ewmh $ docks $ def
                  { manageHook = myManageHook <+> manageDocks
                  , handleEventHook = trayerPaddingXmobarEventHook
                                      <+> swallowEventHook (className =? "Alacritty" <||> className =? "st-256color" <||> className =? "XTerm") (return True)
                  , modMask = myModMask
                  , terminal = myTerminal
                  , startupHook = myStartupHook
                  , layoutHook = showWName' myShowWNameTheme $ layouts
                  , workspaces = myWorkspaces
                  , borderWidth = myBorderWidth
                  , normalBorderColor = normColor theme
                  , focusedBorderColor = focusColor theme
                  , logHook = dynamicLogWithPP (bmobarPP theme windowCount (xmobarOutput mobars))
                  }

  launch myConfig xdirs
