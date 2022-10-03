module BMonad.Bar where

import XMonad
import XMonad.Hooks.DynamicLog (xmobarPP, PP(..), xmobarColor, shorten, wrap)

import BMonad.Theme

bmobarPP :: BMonadTheme -> X (Maybe String) -> (String -> IO ()) -> PP
bmobarPP t wCount outputs = 
  let cCur = currentColor t
      cVis = visibleColor t
      cHid = hiddenColor t
      cHidN = hiddenNoWinColor t
      cSep = sepColor t
      cTitle = titleColor t
      cUrg = urgentColor t
   in xmobarPP
     { ppOutput = outputs
     , ppCurrent = xmobarColor cCur "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ cCur ++ ">") "</box>"
     , ppVisible = xmobarColor cVis ""
     , ppHidden = xmobarColor cHid "" . wrap ("<box type=Top width=2 mb=2 color=" ++ cHid ++ ">") "</box>"
     , ppHiddenNoWindows = xmobarColor cHidN ""
     , ppTitle = xmobarColor cTitle "" . shorten 60
     , ppSep = "<fc=" ++ cSep ++ "> <fn=1>|</fn> </fc>"
     , ppUrgent = xmobarColor cUrg "" . wrap "!" "!"
     , ppExtras = [wCount]
     , ppOrder = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]
     }
