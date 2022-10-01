module BMonad.KeyBindings (myKeys, showKeybindings) where

import           BMonad.Applications          (BCategory, gridSelect')
import           BMonad.Variables

import           Data.Char                    (toUpper)

import           XMonad
import           XMonad.Actions.CopyWindow    (kill1)
import           XMonad.Actions.CycleWS       (Direction1D (..), WSType (..),
                                               moveTo, nextScreen, prevScreen,
                                               shiftTo)
import           XMonad.Actions.GridSelect
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

myKeys :: [BCategory] -> XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys cx c =
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks in

    subKeys "XMonad Essentials"
    [ ("M-S-q", addName "Quit XMonad"          $ io exitSuccess)
    , ("M-S-c", addName "Kill focused window"  $ kill1)
    , ("M-S-a", addName "Kill all windows"     $ killAll)
    , ("M-S-<Return>", addName "Run prompt"    $ spawn "~/.local/bin/dm-run")
    , ("M-<Return>", addName "Launch terminal" $ spawn myTerminal)
    ]

    ^++^ subKeys "GridSelect"
    [ ("C-g a", addName "Select favorite apps" $ spawnSelected' $ gridSelect' "favorites" cx)
    , ("C-g g", addName "Select games"         $ spawnSelected' $ gridSelect' "games" cx)
    , ("C-g i", addName "Select internet apps" $ spawnSelected' $ gridSelect' "internet" cx)
    , ("C-g o", addName "Select office apps"   $ spawnSelected' $ gridSelect' "office" cx)
    , ("C-g u", addName "Select settings apps" $ spawnSelected' $ gridSelect' "settings" cx)
    , ("C-g s", addName "Select system apps"   $ spawnSelected' $ gridSelect' "system" cx)
    ]

    ^++^ subKeys "Workspace"
    [ ("M-1", addName "Switch to workspace 1" $ (windows $ W.greedyView $ myWorkspaces !! 0))
    , ("M-2", addName "Switch to workspace 2" $ (windows $ W.greedyView $ myWorkspaces !! 1))
    , ("M-3", addName "Switch to workspace 3" $ (windows $ W.greedyView $ myWorkspaces !! 2))
    , ("M-4", addName "Switch to workspace 4" $ (windows $ W.greedyView $ myWorkspaces !! 3))
    , ("M-S-1", addName "Send to workspace 1" $ (windows $ W.shift      $ myWorkspaces !! 0))
    , ("M-S-2", addName "Send to workspace 2" $ (windows $ W.shift      $ myWorkspaces !! 1))
    , ("M-S-3", addName "Send to workspace 3" $ (windows $ W.shift      $ myWorkspaces !! 2))
    , ("M-S-4", addName "Send to workspace 4" $ (windows $ W.shift      $ myWorkspaces !! 3))
    , ("M-S-<Page_Up>", addName "Move window to next WS and follow"     $ shiftTo Next nonNSP >> moveTo Next nonNSP)
    , ("M-S-<Page_Down>", addName "Move window to prev WS and follow"   $ shiftTo Prev nonNSP >> moveTo Prev nonNSP)
    ]

    ^++^ subKeys "Windows"
    [ ("M-<Up>", addName "Move focused window up"                  $ sendMessage (MoveUp 10))
    , ("M-<Down>", addName "Move focused window down"              $ sendMessage (MoveDown 10))
    , ("M-<Left>", addName "Move focused window left"              $ sendMessage (MoveLeft 10))
    , ("M-<Right>", addName "Move focused window right"            $ sendMessage (MoveRight 10))
    , ("M-m", addName "Move focus to master window"                $ windows W.focusMaster)
    , ("M-j", addName "Move focus to next window"                  $ windows W.focusDown)
    , ("M-k", addName "Move focus to prev window"                  $ windows W.focusUp)
    , ("M-S-j", addName "Swap the focused window with next window" $ windows W.swapDown)
    , ("M-S-k", addName "Swap the focused window with prev window" $ windows W.swapUp)
    , ("M-<Backspace", addName "Move focused window to master"     $ promote)
    ]

    ^++^ subKeys "Monitors"
    [ ("M-.", addName "Switch focus to next monitor" $ nextScreen)
    , ("M-,", addName "Switch focus to prev monitor" $ prevScreen)
    ]

    ^++^ subKeys "Multimedia keys"
    [ ("<XF86AudioPlay>", addName "mocp play" $ spawn "mocp --play")
    , ("<XF86AudioPrev>", addName "mocp prev" $ spawn "mocp --previous")
    , ("<XF86AudioNext>", addName "mocp next" $ spawn "mocp --next")
    , ("<XF86AudioMute>", addName "Toggle audio mute" $ spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", addName "Lower vol" $ spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", addName "Raise vol" $ spawn "amixer set Master 5%+ unmute")
    ]

  where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0, 0), NamedAction $ map toUpper $ sep ++ "\n-- " ++ x ++ "--\n" ++ sep)
  where sep = replicate (6 + length x) '-'

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def { gs_cellheight = 40
                   , gs_cellwidth = 180
                   , gs_cellpadding = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font = myFont
                   }

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  hPutStr h (unlines $ showKmSimple x)
  hClose h
  return ()
