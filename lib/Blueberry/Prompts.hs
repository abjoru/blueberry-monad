module Blueberry.Prompts where

import Blueberry.Palette
import Blueberry.Variables

import Data.Char (isSpace)

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Util.Run (runProcessWithInput)

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
  { font                = "xft:Mononoki Nerd Font:size=9"
  , bgColor             = pBG0
  , fgColor             = pFG0
  , bgHLight            = pBlue0
  , fgHLight            = "#00000"
  , borderColor         = pBG3
  , promptBorderWidth   = 0
  , promptKeymap        = defaultXPKeymap
  , position            = Top
  , height              = 20
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  , showCompletionOnTab = False
  , alwaysHighlight     = True
  , maxComplRows        = Nothing -- set to Just 5 for 5 rows
  }

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)                 -- manpages prompt
             , ("p", passPrompt)                -- get passwords (requires 'pass')
             , ("g", passGeneratePrompt)        -- generate passwords (requires 'pass')
             , ("r", passRemovePrompt)          -- remove passwords (requires 'pass')
             , ("s", sshPrompt)                 -- ssh prompt
             , ("x", xmonadPrompt)              -- xmonad prompt
             ]

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = 
  inputPrompt c (trim ans) ?+ \input ->
    liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where f = reverse . dropWhile isSpace
