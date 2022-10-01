module BMonad.ManageHooks where

import XMonad

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
  [ [isDialog --> doCenterFloat]
  , [isFullscreen --> doFullFloat]
  , [title =? t --> doFloat / t <- myTFloats]
  , [title =? t --> doShift (head myWorkspaces) / t <- myT0Shifts]
  , [title =? t --> doShift (myWorkspaces !! 1) / t <- myT1Shifts]
  , [title =? t --> doShift (myWorkspaces !! 2) / t <- myT2Shifts]
  , [title =? t --> doShift (myWorkspaces !! 3) / t <- myT3Shifts]
  , [className =? c --> doCenterFloat / c <- myCFloats]
  , [className =? t --> doShift (head myWorkspaces) / t <- myC0Shifts]
  , [className =? t --> doShift (myWorkspaces !! 1) / t <- myC1Shifts]
  , [className =? t --> doShift (myWorkspaces !! 2) / t <- myC2Shifts]
  , [className =? t --> doShift (myWorkspaces !! 3) / t <- myC3Shifts]
  , [resource =? r --> doFloat / r <- myRFloats]
  , [resource =? r --> doIgnore / i <- myIFloats]
  ]
    where myTFloats  = [ "Downloads"
                       , "Save As..."
                       , "Oracle VM VirtualBox Manager"
                       , "MEGAsync"
                       ]
          myT0Shifts = []
          myT1Shifts = ["firefox"]
          myT2Shifts = []
          myCFloats  = [ "confirm"
                       , "file_progress"
                       , "download"
                       , "error"
                       , "notification"
                       , "Arandr"
                       , "Gimp"
                       , "Galculator"
                       , "feh"
                       , "mpv"
                       , "streamdeck"
                       , "megasync", "MEGAsync"
                       ]
          myC0Shifts = []
          myC1Shifts = []
          myC2Shifts = []
          myRFloats  = []
          myIFloats  = ["desktop_window"]
