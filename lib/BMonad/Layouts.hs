{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module BMonad.Layouts (myLayouts) where

import           XMonad
import           XMonad.Actions.MouseResize
import           XMonad.Hooks.ManageDocks            (avoidStruts)

import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT), mkToggle,
                                                      single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed               (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..),
                                                      windowArrange)
import           XMonad.Layout.WindowNavigation

import           BMonad.Theme

-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall :: BMonadTheme -> _
tall c = renamed [Replace "tall"]
       $ limitWindows 5
       $ smartBorders
       $ windowNavigation
       $ addTabs shrinkText (tabTheme c)
       $ subLayout [] (smartBorders Simplest)
       $ mySpacing 8
       $ ResizableTall 1 (3/100) (1/2) []

floats = renamed [Replace "floats"]
         $ smartBorders
         $ simplestFloat

grid :: BMonadTheme -> _
grid c = renamed [Replace "grid"]
       $ limitWindows 9
       $ smartBorders
       $ windowNavigation
       $ addTabs shrinkText (tabTheme c)
       $ subLayout [] (smartBorders Simplest)
       $ mySpacing 8
       $ mkToggle (single MIRROR)
       $ Grid (16/10)

tabs :: BMonadTheme -> _
tabs c = renamed [Replace "tabs"]
       $ tabbed shrinkText (tabTheme c)

myLayouts :: BMonadTheme -> _
myLayouts c = avoidStruts
            $ mouseResize
            $ windowArrange
            $ T.toggleLayouts floats
            $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where myDefaultLayout = withBorder (themeBorderWidth c) (tall c)
                            ||| floats
                            ||| noBorders (tabs c)
                            ||| grid c
