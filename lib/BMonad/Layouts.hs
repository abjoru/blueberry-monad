{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module BMonad.Layouts (bLayoutHook) where

import           XMonad
import           XMonad.Actions.MouseResize
import           XMonad.Hooks.ManageDocks            (avoidStruts)

import           XMonad.Layout.Accordion
import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT), mkToggle,
                                                      single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed               (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..),
                                                      windowArrange)
import           XMonad.Layout.WindowNavigation

import           BMonad.Theme

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall :: BMonadTheme -> _
tall c = renamed [Replace "tall"]
       $ limitWindows 5
       $ smartBorders
       $ windowNavigation
       $ addTabs shrinkText (tabTheme c)
       $ subLayout [] (smartBorders Simplest)
       $ mySpacing 8
       $ ResizableTall 1 (3/100) (1/2) []

monocle :: BMonadTheme -> _
monocle c = renamed [Replace "monocle"]
          $ smartBorders
          $ windowNavigation
          $ addTabs shrinkText (tabTheme c)
          $ subLayout [] (smartBorders Simplest)
          $ Full

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

spirals :: BMonadTheme -> _
spirals c = renamed [Replace "spirals"]
          $ limitWindows 9
          $ smartBorders
          $ windowNavigation
          $ addTabs shrinkText (tabTheme c)
          $ subLayout [] (smartBorders Simplest)
          $ mySpacing' 8
          $ spiral (6/7)

threeCol :: BMonadTheme -> _
threeCol c = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText (tabTheme c)
           $ subLayout [] (smartBorders Simplest)
           $ ThreeCol 1 (3/100) (1/2)

threeRow :: BMonadTheme -> _
threeRow c = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText (tabTheme c)
           $ subLayout [] (smartBorders Simplest)
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)

tabs :: BMonadTheme -> _
tabs c = renamed [Replace "tabs"]
       $ tabbed shrinkText (tabTheme c)

tallAccordion = renamed [Replace "tallAccordion"]
                $ Accordion

wideAccordion = renamed [Replace "wideAccordion"]
                $ Mirror Accordion

bLayoutHook :: BMonadTheme -> _
bLayoutHook c = avoidStruts
             $ mouseResize
             $ windowArrange
             $ T.toggleLayouts floats
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where myDefaultLayout = withBorder (themeBorderWidth c) (tall c)
                            ||| noBorders (monocle c)
                            ||| floats
                            ||| noBorders (tabs c)
                            ||| grid c
                            ||| spirals c
                            ||| threeCol c
                            ||| threeRow c
                            ||| tallAccordion
                            ||| wideAccordion
