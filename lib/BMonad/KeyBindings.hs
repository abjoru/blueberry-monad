module BMonad.KeyBindings (bmonadKeys, showKeys) where

import           BMonad.Config                       (Config (cfgTerminal, cfgWorkspaces),
                                                      bmonadAppGroup,
                                                      bmonadGames,
                                                      spawnSelectedIO)

import           Data.Char                           (toUpper)

import           System.Exit                         (exitSuccess)
import           System.IO                           (hClose, hPutStr)

import           XMonad                              (ChangeLayout (NextLayout),
                                                      KeyMask, KeySym, XConfig,
                                                      io, sendMessage, spawn,
                                                      windows)
import           XMonad.Actions.CopyWindow           (kill1)
import           XMonad.Actions.CycleWS              (Direction1D (Next, Prev),
                                                      WSType (WSIs), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftTo)
import           XMonad.Actions.Promote              (promote)
import           XMonad.Actions.WithAll              (killAll)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts))
import qualified XMonad.Layout.MultiToggle           as MT
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (MoveDown, MoveLeft, MoveRight, MoveUp))
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Tall
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig                (mkNamedKeymap)
import           XMonad.Util.NamedActions            (NamedAction (..), addName,
                                                      showKmSimple, (^++^))
import           XMonad.Util.Run                     (spawnPipe)

bmonadKeys :: Config -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
bmonadKeys b c = keysEssentials b c
  ^++^ keysGridSelect b c
  ^++^ keysDMenu c
  ^++^ keysWorkspaces b c
  ^++^ keysWindows c
  ^++^ keysLayouts c
  ^++^ keysMonitors c
  ^++^ keysMultimedia c

showKeys :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeys x = addName "Show keybindings" $ io $ do
  h <- spawnPipe $ unwords [ "yad"
                           , "--text-info"
                           , "--fontname=\"SauceCodePro Nerd Font Mono 12\""
                           , "--fore=#d79921"
                           , "back=#282828"
                           , "--center"
                           , "--geometry=1200x800"
                           , "--title \"BMonad Keybindings\""
                           ]
  hPutStr h (unlines $ showKmSimple x)
  hClose h
  return ()

{-----------------------------------------------
  Bindings
-----------------------------------------------}

keysEssentials :: Config -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysEssentials b c =
  subKeys c "XMonad Essentials"
    [ ("M-S-q", addName "Quit XMonad" $ io exitSuccess)
    , ("M-S-c", addName "Kill focused window" kill1)
    , ("M-S-a", addName "Kill all windows" killAll)
    , ("M-<Return>", addName "Launch terminal" $ spawn (cfgTerminal b))
    , ("M-S-<Return>", addName "Run prompt" $ spawn "~/.local/bin/dm-run")
    ]

keysGridSelect :: Config -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysGridSelect b c =
  subKeys c "GridSelect"
    [ ("C-g f", addName "Select favorite apps" $ spawnSelectedIO b $ bmonadAppGroup b "favorites")
    , ("C-g g", addName "Select games" $ spawnSelectedIO b $ bmonadGames b)
    , ("C-g i", addName "Select internet apps" $ spawnSelectedIO b $ bmonadAppGroup b "internet")
    , ("C-g o", addName "Select office apps" $ spawnSelectedIO b $ bmonadAppGroup b "office")
    , ("C-g u", addName "Select setting apps" $ spawnSelectedIO b $ bmonadAppGroup b "settings")
    , ("C-g s", addName "Select system apps" $ spawnSelectedIO b $ bmonadAppGroup b "system")
    ]

keysDMenu :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysDMenu c =
  subKeys c "DMenu scripts"
    [ ("M-p p", addName "Passmenu" $ spawn "passmenu -p \"Pass: \"")
    , ("M-p e", addName "Edit config file" $ spawn "dm-confedit")
    , ("M-p k", addName "Kill processes" $ spawn "dm-kill")
    , ("M-p m", addName "View manpages" $ spawn "dm-man")
    , ("M-p r", addName "Online radio" $ spawn "dm-radio")
    , ("M-p s", addName "Web search" $ spawn "dm-websearch")
    , ("M-p x", addName "Open project" $ spawn "bb-projects")
    ]

keysWorkspaces :: Config -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysWorkspaces b c =
  subKeys c "Workspaces"
    [ ("M-S-<Page_Up>", addName "Move window to next WS and follow" $ shiftTo Next nonNSP >> moveTo Next nonNSP)
    , ("M-S-<Page_Down>", addName "Move window to prev WS and follow" $ shiftTo Prev nonNSP >> moveTo Prev nonNSP)
    , ("M-1", addName "Switch to workspace 1" (windows $ W.greedyView $ head (cfgWorkspaces b)))
    , ("M-2", addName "Switch to workspace 2" (windows $ W.greedyView $ cfgWorkspaces b !! 1))
    , ("M-3", addName "Switch to workspace 3" (windows $ W.greedyView $ cfgWorkspaces b !! 2))
    , ("M-4", addName "Switch to workspace 4" (windows $ W.greedyView $ cfgWorkspaces b !! 3))
    , ("M-5", addName "Switch to workspace 5" (windows $ W.greedyView $ cfgWorkspaces b !! 4))
    , ("M-6", addName "Switch to workspace 6" (windows $ W.greedyView $ cfgWorkspaces b !! 5))
    , ("M-7", addName "Switch to workspace 7" (windows $ W.greedyView $ cfgWorkspaces b !! 6))
    , ("M-8", addName "Switch to workspace 8" (windows $ W.greedyView $ cfgWorkspaces b !! 7))
    , ("M-S-1", addName "Send to workspace 1" (windows $ W.shift $ head (cfgWorkspaces b)))
    , ("M-S-2", addName "Send to workspace 2" (windows $ W.shift $ cfgWorkspaces b !! 1))
    , ("M-S-3", addName "Send to workspace 3" (windows $ W.shift $ cfgWorkspaces b !! 2))
    , ("M-S-4", addName "Send to workspace 4" (windows $ W.shift $ cfgWorkspaces b !! 3))
    , ("M-S-5", addName "Send to workspace 5" (windows $ W.shift $ cfgWorkspaces b !! 4))
    , ("M-S-6", addName "Send to workspace 6" (windows $ W.shift $ cfgWorkspaces b !! 5))
    , ("M-S-7", addName "Send to workspace 7" (windows $ W.shift $ cfgWorkspaces b !! 6))
    , ("M-S-8", addName "Send to workspace 8" (windows $ W.shift $ cfgWorkspaces b !! 7))
    ]
  where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

keysWindows :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysWindows c =
  subKeys c "Windows"
    [ ("M-<Up>", addName "Move focused window up" $ sendMessage (MoveUp 10))
    , ("M-<Down>", addName "Move focused window down" $ sendMessage (MoveDown 10))
    , ("M-<Left>", addName "Move focused window left" $ sendMessage (MoveLeft 10))
    , ("M-<Right>", addName "Move focused window right" $ sendMessage (MoveRight 10))
    , ("M-m", addName "Move focus to master window" $ windows W.focusMaster)
    , ("M-j", addName "Move focus to next window" $ windows W.focusDown)
    , ("M-k", addName "Move focus to prev window" $ windows W.focusUp)
    , ("M-S-j", addName "Swap focused window with next window" $ windows W.swapDown)
    , ("M-S-k", addName "Swap focused window with prev window" $ windows W.swapUp)
    , ("M-<Backspace>", addName "Move focused window to master" promote)
    ]

keysLayouts :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysLayouts c =
  subKeys c "Layouts"
    [ ("M-<Tab>", addName "Switch to next layout" $ sendMessage NextLayout)
    , ("M-<Space>", addName "Toggle noborders/full" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    ]

keysMonitors :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysMonitors c =
  subKeys c "Monitors"
    [ ("M-.", addName "Switch focus to next monitor" nextScreen)
    , ("M-,", addName "Switch focus to prev monitor" prevScreen)
    , ("M-F1", layoutScreens 3 (Tall 1 (3/100) (1/3)))
    , ("M-F2", rescreen)
    ]

keysMultimedia :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysMultimedia c =
  subKeys c "Multimedia keys"
    [ ("<XF86AudioPlay>", addName "mocp play" $ spawn "mocp --play")
    , ("<XF86AudioNext>", addName "mocp next" $ spawn "mocp --next")
    , ("<XF86AudioPrev>", addName "mocp prev" $ spawn "mocp --previous")
    , ("<XF86AudioMute>", addName "Toggle mute" $ spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", addName "Lower vol" $ spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", addName "Raise vol" $ spawn "amixer set Master 5%+ unmute")
    ]

{-----------------------------------------------
  Utilities
-----------------------------------------------}

subKeys :: XConfig l -> String -> [(String, NamedAction)] -> [((KeyMask, KeySym), NamedAction)]
subKeys c str ks = subtitle' str : mkNamedKeymap c ks

subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0, 0), NamedAction $ map toUpper $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where sep = replicate (6 + length x) '-'
