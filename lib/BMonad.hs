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
import           XMonad.Layout.Renamed               (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile         (ResizableTall (ResizableTall))
import           XMonad.Layout.Simplest              (Simplest (Simplest))
import           XMonad.Layout.SimplestFloat         (simplestFloat)
import           XMonad.Layout.Spacing               (Border (Border), Spacing,
                                                      spacingRaw)
import           XMonad.Layout.SubLayouts            (subLayout)
import           XMonad.Layout.Tabbed                (addTabs, shrinkText,
                                                      tabbed)
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
                        ||| floats
                        ||| noBorders (tabs cfg)
                        ||| grid cfg

bmonadManageHook :: Config -> Query (Endo WindowSet)
bmonadManageHook cfg = composeAll . concat $
  [ [isDialog --> doCenterFloat]
  , [isFullscreen --> doFullFloat]
  , [title =? t --> doFloat | t <- tfloats]
  , [title =? t --> doShift (head $ cfgWorkspaces cfg) | t <- t0shifts]
  , [title =? t --> doShift (cfgWorkspaces cfg !! 1) | t <- t1shifts]
  , [title =? t --> doShift (cfgWorkspaces cfg !! 2) | t <- t2shifts]
  , [title =? t --> doShift (cfgWorkspaces cfg !! 3) | t <- t3shifts]
  , [className =? c --> doCenterFloat | c <- cfloats]
  , [className =? c --> doShift (head $ cfgWorkspaces cfg) | c <- c0shifts]
  , [className =? c --> doShift (cfgWorkspaces cfg !! 1) | c <- c1shifts]
  , [className =? c --> doShift (cfgWorkspaces cfg !! 2) | c <- c2shifts]
  , [className =? c --> doShift (cfgWorkspaces cfg !! 3) | c <- c3shifts]
  , [resource =? r --> doFloat | r <- rfloats]
  , [resource =? i --> doIgnore | i <- ifloats]
  ]
  where tfloats  = [ "Downloads"
                   , "Save As..."
                   , "Oracle VM VirtualBox Manager"
                   , "MEGAsync"
                   ]
        t0shifts = []
        t1shifts = ["firefox"]
        t2shifts = []
        t3shifts = []
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
                   ]
        c0shifts = []
        c1shifts = []
        c2shifts = []
        c3shifts = []
        rfloats  = []
        ifloats  = ["desktop_window"]

{---------------------------------------------
  Layouts
---------------------------------------------}

bspacing :: Integer -> l a -> ModifiedLayout Spacing l a
bspacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall :: Config -> _
tall cfg = renamed [Replace "tall"]
         $ limitWindows 5
         $ smartBorders
         $ windowNavigation
         $ addTabs shrinkText (bmonadTabTheme cfg)
         $ subLayout [] (smartBorders Simplest)
         $ bspacing 8
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
         $ bspacing 8
         $ mkToggle (single MIRROR)
         $ Grid (16/10)

tabs :: Config -> _
tabs cfg = renamed [Replace "tabs"]
         $ tabbed shrinkText (bmonadTabTheme cfg)
