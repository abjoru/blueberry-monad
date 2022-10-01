module BMonad.Variables where

import           System.Directory           (XdgDirectory (XdgConfig),
                                             getHomeDirectory, getXdgDirectory)
import           System.FilePath
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import BMonad.Colors.GruvboxDark

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
myNormColor :: String
myNormColor = colorBack

-- border color of focused windows
myFocusColor :: String
myFocusColor = color15

-- setting this for use in xprompts
altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- workspace naming and indices
myWorkspaces = [" dev ", " www ", " sys ", " gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- setting color for tabs layout and tabs sublayout
myTabTheme = def 
  { fontName            = myFont
  , activeColor         = color15
  , inactiveColor       = color08
  , activeBorderColor   = color15
  , inactiveBorderColor = colorBack
  , activeTextColor     = colorBack
  , inactiveTextColor   = color16
  }

-- theme for showWName which prints current workspace when you change
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font    = "xft:Ubuntu:bold:size=60"
  , swn_fade    = 1.0
  , swn_bgcolor = "#1c1f24"
  , swn_color   = "#ffffff"
  }
