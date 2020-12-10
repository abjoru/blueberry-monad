module Main where

-- Blueberry modules
import Blueberry.Palette
import Blueberry.Variables
import Blueberry.Layouts
import Blueberry.KeyBindings

-- Base
import XMonad

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (docksEventHook, manageDocks)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.WorkspaceHistory

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe, hPutStrLn)

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

----------
-- MAIN --
----------
main :: IO ()
main = do
  home   <- getHomeDirectory
  mobars <- countScreens >>= spawnBerrybars (home </> ".local" </> "bin" </> "blueberry-mobar")
  launch $ ewmh $ def
    { manageHook = (isFullscreen --> doFullFloat) <+> myManageHook <+> manageDocks
    , handleEventHook = serverModeEventHookCmd
      <+> serverModeEventHook
      <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
      <+> docksEventHook
    , modMask = myModMask
    , terminal = myTerminal
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , workspaces = myWorkspaces
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                { ppOutput = xmobarOutput mobars
                , ppCurrent = xmobarColor pGreen0 "" . wrap "[" "]"   -- current workspace in xmobar
                , ppVisible = xmobarColor pGreen1 ""                  -- visible but not current workspace
                , ppHidden = xmobarColor pBlue0 "" . wrap "*" ""      -- hidden workspaces in xmobar
                , ppHiddenNoWindows = xmobarColor pYellow0 ""         -- Hidden workspaces (no window)
                , ppTitle = xmobarColor pGray0 "" . shorten 60        -- Title of active window in xmobar
                , ppSep = " | "
                , ppUrgent = xmobarColor pOrange0 "" . wrap "!" "!"   -- Urgent windows
                , ppExtras = [windowCount]                            -- # of windows in current workspace
                , ppOrder = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]
                }
    } `additionalKeysP` myKeys
