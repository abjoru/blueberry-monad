module BMonad.KeyBindings (bmonadKeys, showKeys) where

import           BMonad.Config                       (Config (cfgTerminal, cfgWorkspaces),
                                                      bmonadAppGroup,
                                                      bmonadGames,
                                                      spawnSelectedIO)

import           Data.Char                           (toUpper)

import           System.Exit                         (exitSuccess)
import           System.IO                           (hClose, hPutStr)

import           XMonad                              (ChangeLayout (NextLayout),
                                                      KeyMask, KeySym,
                                                      Tall (Tall), XConfig, io,
                                                      rescreen, sendMessage,
                                                      spawn, windows,
                                                      withFocused)
import           XMonad.Actions.CopyWindow           (kill1)
import           XMonad.Actions.CycleWS              (Direction1D (Next, Prev),
                                                      WSType (WSIs), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftTo)
import           XMonad.Actions.Promote              (promote)
import           XMonad.Actions.WithAll              (killAll)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts))
import           XMonad.Layout.LayoutScreens         (layoutScreens)
import qualified XMonad.Layout.MultiToggle           as MT
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (MoveDown, MoveLeft, MoveRight, MoveUp))
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig                (mkNamedKeymap)
import           XMonad.Util.NamedActions            (NamedAction (..), addName,
                                                      showKmSimple, (^++^))
import           XMonad.Util.Run                     (spawnPipe)

bmonadKeys :: Config -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
bmonadKeys b c = keysEssentials b c
  ^++^ keysGridSelect b c
  -- ^++^ keysDMenu c
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
                           , "--back=#282828"
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
    , ("M-S-<Return>", addName "Run prompt" $ spawn "rofi -show drun")
    , ("M-S-p", addName "Lookup passwords" $ spawn "~/.config/xmonad/scripts/rofi_askpass")
    , ("M-C-a", addName "Toggle dictation" $ spawn "scribe toggle")
    , ("<F13>", addName "Toggle dictation (foot pedal)" $ spawn "scribe toggle")
    , ("S-<F13>", addName "Cancel dictation (foot pedal + shift)" $ spawn "scribe cancel")
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

keysWorkspaces :: Config -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
keysWorkspaces b c =
  subKeys c "Workspaces" $
    [ ("M-S-<Page_Up>", addName "Move window to next WS and follow" $ shiftTo Next nonNSP >> moveTo Next nonNSP)
    , ("M-S-<Page_Down>", addName "Move window to prev WS and follow" $ shiftTo Prev nonNSP >> moveTo Prev nonNSP)
    ]
    ++ zipWith mkView [1..] (cfgWorkspaces b)
    ++ zipWith mkShift [1..] (cfgWorkspaces b)
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    mkView :: Int -> String -> (String, NamedAction)
    mkView n ws = ("M-" ++ show n, addName ("Switch to " ++ ws) (windows $ W.greedyView ws))
    mkShift :: Int -> String -> (String, NamedAction)
    mkShift n ws = ("M-S-" ++ show n, addName ("Send to " ++ ws) (windows $ W.shift ws))

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
    , ("M-t", addName "Retile focused window" $ withFocused $ windows . W.sink)
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
    , ("M-f", addName "Set resolution to full" $ spawn "xrandr --output DisplayPort-0 --mode 5120x1440 --rate 120")
    , ("M-S-f", addName "Set resolution to half" $ spawn "xrandr --output DisplayPort-0 --mode 2560x1440 --rate 120")
    -- FIXME the following doesn't seem to work!
    , ("M-S-F1", addName "Reset screens" rescreen)
    , ("M-S-F2", addName "Split into 2 screens" $ layoutScreens 2 (Tall 1 (3/100) (1/2)))
    , ("M-S-F3", addName "Split into 3 screens" $ layoutScreens 3 (Tall 1 (3/100) (1/3)))
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
