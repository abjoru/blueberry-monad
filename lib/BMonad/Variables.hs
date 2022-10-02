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

import           BMonad.Colors

-- default configuration directories
myXMonadDir, myXMobarDir :: IO FilePath
myXMonadDir = getXdgDirectory XdgConfig "xmonad"
myXMobarDir = getXdgDirectory XdgConfig "xmobar"

-- sets the desktop font
myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=11;antialias=true:hinting=true"
--myFont = "xft:Mononoki Nerd Font:bold:pixelsize=11"

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

-- sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- border color of normal windows
normColor :: Colors -> String
normColor = colorBack

-- border color of focused windows
focusColor :: Colors -> String
focusColor = color15

-- setting this for use in xprompts
altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- workspace naming and indices
myWorkspaces = [" dev ", " www ", " sys ", " gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- setting color for tabs layout and tabs sublayout
tabTheme :: Colors -> Theme
tabTheme c = def
  { fontName            = myFont
  , activeColor         = color15 c
  , inactiveColor       = color08 c
  , activeBorderColor   = color15 c
  , inactiveBorderColor = colorBack c
  , activeTextColor     = colorBack c
  , inactiveTextColor   = color16 c
  }

-- theme for showWName which prints current workspace when you change
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font    = "xft:Ubuntu:bold:size=60"
  , swn_fade    = 1.0
  , swn_bgcolor = "#1c1f24"
  , swn_color   = "#ffffff"
  }

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 1

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "$HOME/.config/xmonad/scripts/autostart.sh"
  setWMName "XBlueberry"
