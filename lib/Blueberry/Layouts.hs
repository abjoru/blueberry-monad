module Blueberry.Layouts where

import Blueberry.Palette

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts)

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layout Modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.ShowWName
import XMonad.Layout.WindowArranger (windowArrange)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts)

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------

-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 6
           $ ResizableTall 1 (3/100) (1/2) []

magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 6
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ limitWindows 20
           $ Full

floats   = renamed [Replace "floats"]
           $ limitWindows 20
           $ simplestFloat

grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 6
           $ mkToggle (single MIRROR)
           $ Grid (16/10)

spirals  = renamed [Replace "spirals"]
           $ mySpacing' 6
           $ spiral (6/7)

threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)

threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad
           $ tabbed shrinkText myTabConfig
           where myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                                   , activeColor         = pBlue0
                                   , inactiveColor       = pBG3
                                   , activeBorderColor   = pBlue0
                                   , inactiveBorderColor = pBlue0
                                   , activeTextColor     = pFG0
                                   , inactiveTextColor   = pGray0
                                   }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font    = "xft:Sans:bold:size=60"
  , swn_fade    = 1.0
  , swn_bgcolor = pBG0 
  , swn_color   = pFG0 
  }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
                mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
        where
          myDefaultLayout = tall ||| magnify ||| noBorders monocle ||| floats ||| noBorders tabs
