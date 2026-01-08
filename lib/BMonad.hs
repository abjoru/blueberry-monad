{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module BMonad (
  module BMonad.Bar,
  module BMonad.Config,
  module BMonad.Crypto,
  module BMonad.Log,
  module BMonad.KeyBindings,

  bmonadLayout,
  bmonadManageHook
) where

import           BMonad.Bar
import           BMonad.Config
import           BMonad.Crypto
import           BMonad.KeyBindings
import           BMonad.Log

import           Data.Monoid                         (Endo)

import           BMonad.Config.Types
import           XMonad
import           XMonad.Actions.MouseResize          (mouseResize)
import           XMonad.Hooks.ManageDocks            (avoidStruts)
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doFullFloat, isDialog,
                                                      isFullscreen)
import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.LayoutModifier        (ModifiedLayout)
import           XMonad.Layout.LimitWindows          (limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT), mkToggle,
                                                      single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders             (noBorders, smartBorders,
                                                      withBorder)
import           XMonad.Layout.Reflect               (reflectHoriz)
import           XMonad.Layout.Renamed               (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile         (ResizableTall (ResizableTall))
import           XMonad.Layout.Simplest              (Simplest (Simplest))
import           XMonad.Layout.SimplestFloat         (simplestFloat)
import           XMonad.Layout.Spacing               (Border (Border), Spacing,
                                                      spacingRaw)
import           XMonad.Layout.SubLayouts            (subLayout)
import           XMonad.Layout.Tabbed                (addTabs, shrinkText,
                                                      tabbed)
import           XMonad.Layout.ThreeColumns          (ThreeCol (ThreeColMid))
import qualified XMonad.Layout.ToggleLayouts         as T
import           XMonad.Layout.WindowArranger        (windowArrange)
import           XMonad.Layout.WindowNavigation      (windowNavigation)

bmonadLayout :: Config -> _
bmonadLayout cfg = avoidStruts
                 $ mouseResize
                 $ windowArrange
                 $ T.toggleLayouts floats
                 $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) defaultLayout
  where defaultLayout = withBorder (themeBorderWidth $ cfgTheme cfg) (tall cfg)
                        ||| withBorder (themeBorderWidth $ cfgTheme cfg) (cols cfg)
                        ||| withBorder (themeBorderWidth $ cfgTheme cfg) floats
                        ||| noBorders (tabs cfg)
                        ||| withBorder (themeBorderWidth $ cfgTheme cfg) (grid cfg)

bmonadManageHook :: Config -> Query (Endo WindowSet)
bmonadManageHook cfg = composeAll . concat $
  [ [isDialog --> doCenterFloat]
  , [isFullscreen --> doFullFloat]
  , [title =? t --> doFloat | t <- tfloats]
  , [title =? t --> doShift ws | (ws, ts) <- zip (cfgWorkspaces cfg) tshifts, t <- ts]
  , [className =? c --> doCenterFloat | c <- cfloats]
  , [className =? c --> doShift ws | (ws, cs) <- zip (cfgWorkspaces cfg) cshifts, c <- cs]
  , [resource =? r --> doFloat | r <- rfloats]
  , [resource =? i --> doIgnore | i <- ifloats]
  ]
  where tfloats  = [ "Downloads"
                   , "Save As..."
                   , "Oracle VM VirtualBox Manager"
                   , "MEGAsync"
                   ]
        tshifts  = [[], ["firefox"], [], []]  -- ws0, ws1, ws2, ws3
        cfloats  = [ "confirm"
                   , "file_progress"
                   , "download"
                   , "error"
                   , "notification"
                   , "splash"
                   , "toolbar"
                   , "Arandr"
                   , "Gimp"
                   , "Galculator"
                   , "feh"
                   , "mpv"
                   , "streamdeck"
                   , "megasync"
                   , "pavucontrol"
                   ]
        cshifts  = [[], [], [], []]  -- ws0, ws1, ws2, ws3
        rfloats  = []
        ifloats  = ["desktop_window"]

{---------------------------------------------
  Layouts
---------------------------------------------}

bspacing :: Integer -> l a -> ModifiedLayout Spacing l a
bspacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

cols :: Config -> _
cols cfg = renamed [Replace "cols"]
         $ limitWindows 5
         $ smartBorders
         $ windowNavigation
         $ addTabs shrinkText (bmonadTabTheme cfg)
         $ subLayout [] (smartBorders Simplest)
         $ bspacing (monadWindowSpacing cfg)
         $ ThreeColMid 1 (3/100) (1/3)

tall :: Config -> _
tall cfg = renamed [Replace "tall"]
         $ limitWindows 5
         $ smartBorders
         $ windowNavigation
         $ addTabs shrinkText (bmonadTabTheme cfg)
         $ subLayout [] (smartBorders Simplest)
         $ bspacing (monadWindowSpacing cfg)
         $ reflectHoriz
         $ ResizableTall 1 (3/100) (1/2) []

floats :: _
floats = renamed [Replace "floats"] $ smartBorders simplestFloat

grid :: Config -> _
grid cfg = renamed [Replace "grid"]
         $ limitWindows 9
         $ smartBorders
         $ windowNavigation
         $ addTabs shrinkText (bmonadTabTheme cfg)
         $ subLayout [] (smartBorders Simplest)
         $ bspacing (monadWindowSpacing cfg)
         $ mkToggle (single MIRROR)
         $ Grid (16/10)

tabs :: Config -> _
tabs cfg = renamed [Replace "tabs"]
         $ bspacing (monadWindowSpacing cfg)
         $ tabbed shrinkText (bmonadTabTheme cfg)
