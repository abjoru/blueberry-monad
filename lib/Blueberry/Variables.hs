module Blueberry.Variables where

import Blueberry.Palette

import Data.Monoid

import System.Directory (getHomeDirectory, getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath

import XMonad

import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive

import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W

import XMonad.Util.SpawnOnce

myFont :: [Char]
myFont = "xft:Mononoki Nerd Font:bold:pixelsize=11"

-- sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask 

-- sets default terminal
myTerminal :: [Char]
myTerminal = "alacritty" 

-- sets default browser for tree select
myBrowser :: String
myBrowser = myTerminal ++ " -e lynx "

-- sets default editor for tree select
myEditor :: String
myEditor = myTerminal ++ " -e nvim "

-- sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- border color of normal windows
myNormColor :: [Char]
myNormColor = pBG0

-- border color of focused windows
myFocusColor :: [Char]
myFocusColor = pFG1

-- setting this for use in xprompts
altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Workspace names
myWorkspaces :: [String]
myWorkspaces = ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]

------------------------------------------------------------------------
-- Folder Variables
------------------------------------------------------------------------

-- Game folder
myGamesFolder :: IO FilePath
myGamesFolder = fmap (\x -> x </> "Games") getHomeDirectory

-- Config folders
configDirDotf, configDirXmobar :: IO FilePath
configDirDotf   = getXdgDirectory XdgConfig "dotf"
configDirXmobar = getXdgDirectory XdgConfig "xmobar"

------------------------------------------------------------------------
-- Search Variables
------------------------------------------------------------------------

searchArchwiki, searchEbay, searchReddit, searchUrban :: S.SearchEngine
searchArchwiki  = S.searchEngine "archwiki"     "https://wiki.archlinux.org/index.php?search="
searchEbay      = S.searchEngine "ebay"         "https://www.ebay.com/sch/i.html?_nkw="
searchReddit    = S.searchEngine "reddit"       "https://www.reddit.com/search/?q="
searchUrban     = S.searchEngine "urban"        "https://www.urbandictionary.com/define.php?term="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", searchArchwiki)
             , ("e", searchEbay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("r", searchReddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", searchUrban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

------------------------------------------------------------------------
-- Application/Config Variables
------------------------------------------------------------------------

-- The lists below are actually 3-tuples for use with gridSelect and treeSelect.
-- TreeSelect uses all three values in the 3-tuples but GridSelect only needs first
-- two values in each list
myApplications :: [(String, String, String)]
myApplications = [ ("QuteBrowser", "qutebrowser", "Simple VIM-like web browser")
                 , ("Brave", "brave", "The privacy centered brave web browser")
                 , ("PCManFM", "pcmanfm", "Lightweight graphical file manager")
                 , ("ThinkOrSwim", "thinkorswim", "TD Ameritrade platform")
                 , ("Steam", "steam", "Proprietary gaming platform")
                 , ("PrusaSlicer", "prusa-slicer", "Prusa 3d printer slicer software")
                 , ("VirtualBox", "virtualbox", "Virtualization software")
                 , ("Blender", "blender", "Blender 3D Software")
                 , ("Geary", "geary", "Geary Email Client")
                 , ("Gitter", "gitter", "Gitter Desktop Client")
                 , ("Plex", "plexmediaplayer", "Plex Media Player")
                 ]

myConfigs :: [(String, String, String)]
myConfigs = [ ("xmonad", myEditor ++ "~/.config/xmonad/xmonad.hs", "xmonad config")
            , ("zshrc", myEditor ++ "~/.config/zsh/config.zsh", "zsh config")
            , ("nvim", myEditor ++ "~/.config/nvim/init.vim", "NeoVim main config file")
            ]

myAppGrid :: [(String, String)]
myAppGrid = [ (a,b) | (a,b,_) <- myApplications]

myConfigGrid :: [(String, String)]
myConfigGrid = [ (a,b) | (a,b,_) <- myConfigs]

------------------------------------------------------------------------
-- XMonad Hooks
------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
  [ [isDialog --> doCenterFloat]
  , [className =? c --> doCenterFloat | c <- myCFloats]
  , [title =? t --> doFloat | t <- myTFloats]
  , [resource =? r --> doFloat | r <- myRFloats]
  , [resource =? i --> doIgnore | i <- myIFloats]
  , [className =? c --> doShift (head myWorkspaces) | c <- myC0Shifts]
  , [className =? c --> doShift (myWorkspaces !! 1) | c <- myC1Shifts]
  , [title =? t --> doShift (head myWorkspaces) | t <- myT0Shifts]
  , [title =? t --> doShift (myWorkspaces !! 1) | t <- myT1Shifts]
  ]
  where myCFloats  = ["Arandr", "Gimp", "Galculator", "feh", "mpv", "streamdeck"]
        myTFloats  = ["Downloads", "Save As...", "Oracle VM VirtualBox Manager"]
        myRFloats  = []
        myIFloats  = ["desktop_window"]
        myC0Shifts = ["Oracle VM VirtualBox Manager"]
        myC1Shifts = []
        myT0Shifts = []
        myT1Shifts = ["firefox"]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 1

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "$HOME/.config/xmonad/scripts/autostart.sh"
  setWMName "XBlueberry"
