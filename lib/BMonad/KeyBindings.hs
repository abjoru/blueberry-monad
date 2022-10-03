module BMonad.KeyBindings (myKeys, showKeybindings) where

import           BMonad.Applications          (BCategory, gridSelect')
import           BMonad.GridSelect            (spawnSelected')
import           BMonad.Variables

import           Data.Char                    (toUpper)

import           XMonad
import           XMonad.Actions.CopyWindow    (kill1)
import           XMonad.Actions.CycleWS       (Direction1D (..), WSType (..),
                                               moveTo, nextScreen, prevScreen,
                                               shiftTo)
import           XMonad.Actions.Promote
import           XMonad.Actions.WithAll       (killAll)
import           XMonad.Layout.WindowArranger (WindowArrangerMsg (..))
import           XMonad.Prompt.Shell          (shellPrompt)
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (mkNamedKeymap)
import           XMonad.Util.NamedActions
import           XMonad.Util.Run              (spawnPipe)

import           System.Exit                  (exitSuccess)
import           System.IO                    (hClose, hPutStr, hPutStrLn)

keysEssentials :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysEssentials c = subKeys c "XMonad Essentials"
                   [ ("M-S-q",        addName "Quit XMonad"         $ io exitSuccess)
                   , ("M-S-c",        addName "Kill focused window" $ kill1)
                   , ("M-S-a",        addName "Kill all windows"    $ killAll)
                   , ("M-<Return>",   addName "Launch terminal"     $ spawn myTerminal)
                   , ("M-S-<Return>", addName "Run prompt"          $ spawn "~/.local/bin/dm-run")
                   ]

keysGridSelect :: [BCategory] -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysGridSelect apps c = subKeys c "GridSelect"
                        [ ("C-g f", addName "Select favorite apps" $ spawnSelected' $ gridSelect' "favorites" apps)
                        , ("C-g g", addName "Select games"         $ spawnSelected' $ gridSelect' "games" apps)
                        , ("C-g i", addName "Select internet apps" $ spawnSelected' $ gridSelect' "internet" apps)
                        , ("C-g o", addName "Select office apps"   $ spawnSelected' $ gridSelect' "office" apps)
                        , ("C-g u", addName "Select settings apps" $ spawnSelected' $ gridSelect' "settings" apps)
                        , ("C-g s", addName "Select system apps"   $ spawnSelected' $ gridSelect' "system" apps)
                        ]

keysDMenu :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysDMenu c = subKeys c "DMenu scripts"
              [ ("M-p p", addName "Passmenu"               $ spawn "passmenu -p \"Pass: \"")
              , ("M-p e", addName "Edit config file"       $ spawn "dm-configedit")
              , ("M-p k", addName "Kill processes"         $ spawn "dm-kill")
              , ("M-p m", addName "View manpages"          $ spawn "dm-man")
              , ("M-p r", addName "Listen to online radio" $ spawn "dm-radio")
              , ("M-p s", addName "Search various engines" $ spawn "dm-websearch")
              ]

keysWorkspaces :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysWorkspaces c = subKeys c "Workspaces"
                   [ ("M-1",             addName "Switch to workspace 1"             $ (windows $ W.greedyView $ myWorkspaces !! 0))
                   , ("M-2",             addName "Switch to workspace 2"             $ (windows $ W.greedyView $ myWorkspaces !! 1))
                   , ("M-3",             addName "Switch to workspace 3"             $ (windows $ W.greedyView $ myWorkspaces !! 2))
                   , ("M-4",             addName "Switch to workspace 4"             $ (windows $ W.greedyView $ myWorkspaces !! 3))
                   , ("M-S-1",           addName "Send to workspace 1"               $ (windows $ W.shift      $ myWorkspaces !! 0))
                   , ("M-S-2",           addName "Send to workspace 2"               $ (windows $ W.shift      $ myWorkspaces !! 1))
                   , ("M-S-3",           addName "Send to workspace 3"               $ (windows $ W.shift      $ myWorkspaces !! 2))
                   , ("M-S-4",           addName "Send to workspace 4"               $ (windows $ W.shift      $ myWorkspaces !! 3))
                   , ("M-S-<Page_Up>",   addName "Move window to next WS and follow" $ shiftTo Next nonNSP >> moveTo Next nonNSP)
                   , ("M-S-<Page_Down>", addName "Move window to prev WS and follow" $ shiftTo Prev nonNSP >> moveTo Prev nonNSP)
                   ]
  where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

keysWindows :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysWindows c = subKeys c "Windows"
                [ ("M-<Up>",       addName "Move focused window up"                   $ sendMessage (MoveUp 10))
                , ("M-<Down>",     addName "Move focused window down"                 $ sendMessage (MoveDown 10))
                , ("M-<Left>",     addName "Move focused window left"                 $ sendMessage (MoveLeft 10))
                , ("M-<Right>",    addName "Move focused window right"                $ sendMessage (MoveRight 10))
                , ("M-m",          addName "Move focus to master window"              $ windows W.focusMaster)
                , ("M-j",          addName "Move focus to next window"                $ windows W.focusDown)
                , ("M-k",          addName "Move focus to prev window"                $ windows W.focusUp)
                , ("M-S-j",        addName "Swap the focused window with next window" $ windows W.swapDown)
                , ("M-S-k",        addName "Swap the focused window with prev window" $ windows W.swapUp)
                , ("M-<Backspace", addName "Move focused window to master"            $ promote)
                ]

keysMonitors :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysMonitors c = subKeys c "Monitors"
                 [ ("M-.", addName "Switch focus to next monitor" $ nextScreen)
                 , ("M-,", addName "Switch focus to prev monitor" $ prevScreen)
                 ]

keysMultimedia :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysMultimedia c = subKeys c "Multimedia keys"
                   [ ("<XF86AudioPlay>", addName "mocp play" $ spawn "mocp --play")
                   , ("<XF86AudioPrev>", addName "mocp prev" $ spawn "mocp --previous")
                   , ("<XF86AudioNext>", addName "mocp next" $ spawn "mocp --next")
                   , ("<XF86AudioMute>", addName "Toggle audio mute" $ spawn "amixer set Master toggle")
                   , ("<XF86AudioLowerVolume>", addName "Lower vol" $ spawn "amixer set Master 5%- unmute")
                   , ("<XF86AudioRaiseVolume>", addName "Raise vol" $ spawn "amixer set Master 5%+ unmute")
                   ]

myKeys :: [BCategory] -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
myKeys apps c = keysEssentials c
                ^++^ keysGridSelect apps c
                ^++^ keysWorkspaces c
                ^++^ keysWindows c
                ^++^ keysMultimedia c
                ^++^ keysDMenu c

subKeys :: XConfig l -> String -> [(String, NamedAction)] -> [((KeyMask, KeySym), NamedAction)]
subKeys c str ks = subtitle' str : mkNamedKeymap c ks

subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0, 0), NamedAction $ map toUpper $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ unwords [ "yad"
                           , "--text-info"
                           , "--fontname=\"SauceCodePro Nerd Font Mono 12\""
                           , "--fore=#d79921"
                           , "back=#282828"
                           , "--center"
                           , "--geometry=1200x800"
                           , "--title \"XMonad keybindings\""
                           ]
  hPutStr h (unlines $ showKmSimple x)
  hClose h
  return ()
