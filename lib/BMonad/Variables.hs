module BMonad.Variables where

import qualified Data.Map                     as M
import           System.Directory             (XdgDirectory (XdgConfig),
                                               getHomeDirectory,
                                               getXdgDirectory)
import           System.FilePath
import           XMonad
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet              as W
import           XMonad.Util.SpawnOnce

-- default configuration directories
myXMonadDir, myXMobarDir :: IO FilePath
myXMonadDir = getXdgDirectory XdgConfig "xmonad"
myXMobarDir = getXdgDirectory XdgConfig "xmobar"

-- sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- sets default terminal
myTerminal :: String
myTerminal = "alacritty"

-- sets default browser for tree select
myBrowser :: String
myBrowser = "qutebrowser"

-- sets default editor for tree select
myEditor :: String
myEditor = myTerminal ++ " -e nvim "

-- the program that will play system sounds
mySoundPlayer :: String
mySoundPlayer = "ffplay -nodisp -autoexit"

-- setting this for use in xprompts
altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- workspace naming and indices
myWorkspaces = [" dev ", " www ", " sys ", " gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 1

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "$HOME/.config/xmonad/scripts/autostart.sh"
  setWMName "XBlueberry"
