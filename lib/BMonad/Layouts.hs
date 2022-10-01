module BMonad.Layouts (myLayoutHook) where

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

import           BMonad.Variables                    (myBorderWidth, myFont,
                                                      mySpacing, mySpacing',
                                                      myTabTheme)

tall = renamed [Replace "tall"]
       $ limitWindows 5
       $ smartBorders
       $ windowNavigation
       $ addTabs shrinkText myTabTheme
       $ subLayout [] (smartBorders Simplest)
       $ mySpacing 8
       $ ResizableTall 1 (3/100) (1/2) []

monocle = renamed [Replace "monocle"]
          $ smartBorders
          $ windowNavigation
          $ addTabs shrinkText myTabTheme
          $ subLayout [] (smartBorders Simplest)
          $ Full

floats = renamed [Replace "floats"]
         $ smartBorders
         $ simplestFloat

grid = renamed [Replace "grid"]
       $ limitWindows 9
       $ smartBorders
       $ windowNavigation
       $ addTabs shrinkText myTabTheme
       $ subLayout [] (smartBorders Simplest)
       $ mySpacing 8
       $ mkToggle (single MIRROR)
       $ Grid (16/10)

spirals = renamed [Replace "spirals"]
          $ limitWindows 9
          $ smartBorders
          $ windowNavigation
          $ addTabs shrinkText myTabTheme
          $ subLayout [] (smartBorders Simplest)
          $ mySpacing' 8
          $ spiral (6/7)

threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ ThreeCol 1 (3/100) (1/2)

threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)

tabs = renamed [Replace "tabs"]
       $ tabbed shrinkText myTabTheme

tallAccordion = renamed [Replace "tallAccordion"]
                $ Accordion

wideAccordion = renamed [Replace "wideAccordion"]
                $ Mirror Accordion

myLayoutHook = avoidStruts
               $ mouseResize
               $ windowArrange
               $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where myDefaultLayout = withBorder myBorderWidth tall
                            ||| noBorders monocle
                            ||| floats
                            ||| noBorders tabs
                            ||| grid
                            ||| spirals
                            ||| threeCol
                            ||| threeRow
                            ||| tallAccordion
                            ||| wideAccordion
