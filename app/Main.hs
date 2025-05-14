module Main (main) where

import           BMonad                    
import           BMonad.Utils              (countWindows)

import           XMonad                    (Default (def),
                                            XConfig (borderWidth, focusedBorderColor, handleEventHook, layoutHook, logHook, manageHook, modMask, normalBorderColor, startupHook, terminal, workspaces),
                                            getDirectories, launch, mod4Mask,
                                            xK_F1, (<+>))
import           XMonad.Hooks.DynamicLog   (dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.ManageDocks  (docks, manageDocks)
import           XMonad.Util.Hacks         (trayerPaddingXmobarEventHook)
import           XMonad.Util.NamedActions  (addDescrKeys')
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Tall
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  cfg   <- bmonadConfig
  bars  <- bmonadBars
  xdirs <- getDirectories
  _     <- setupLogger (cfgLogLevel cfg) (cfgMonadDir cfg)

  let bmonad = addDescrKeys' ((mod4Mask, xK_F1), showKeys) (bmonadKeys cfg)
             $ ewmh
             $ docks
             $ def { manageHook = bmonadManageHook cfg <+> manageDocks
                   , handleEventHook = trayerPaddingXmobarEventHook
                   -- TODO active below when input argument is given such
                   -- that we can control when to split the screen. We
                   -- don't want to split on the laptop. Just the ultrawide!
                   --, handleEventHook =   bmonadLayoutScreensHook 
                                     -- <+> trayerPaddingXmobarEventHook 
                                     -- <+> handleEventHook def
                   , modMask = cfgModMask cfg
                   , terminal = cfgTerminal cfg
                   , startupHook = bmonadStartupHook cfg
                   , layoutHook = bmonadLayout cfg
                   , workspaces = cfgWorkspaces cfg
                   , borderWidth = themeBorderWidth $ cfgTheme cfg
                   , normalBorderColor = color09 . themeColorScheme $ cfgTheme cfg
                   , focusedBorderColor = color04 . themeColorScheme $ cfgTheme cfg
                   , logHook = dynamicLogWithPP (bmobarPP cfg countWindows bars)
                   }

  launch bmonad xdirs

-- This is safe in XMonad's pure-ish world because 
-- it's run once at startup
{-# NOINLINE hasRunLayout #-}
hasRunLayout :: IORef Bool
hasRunLayout = unsafePerformIO (newIORef False)

bmonadLayoutScreensHook :: Event -> X All
bmonadLayoutScreensHook (MapRequestEvent {}) = do
  alreadyRun <- io $ readIORef hasRunLayout
  when (not alreadyRun) $ do
    io $ writeIORef hasRunLayout True
    layoutScreens 3 (Tall 1 (3/100) (1/3))
  return (All True)
bmonadLayoutScreensHook _ = return (All True)
