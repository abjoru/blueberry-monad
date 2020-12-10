module Blueberry.Scratchpads where

import Blueberry.Variables

import XMonad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
		, NS "cmus" spawnCmus findCmus manageCmus
		]
	where
	  spawnTerm  = myTerminal ++ " -n scratchpad"
	  findTerm   = resource =? "scratchpad"
	  manageTerm = customFloating $ W.RationalRect l t w h
		where
		  h = 0.9
		  w = 0.9
		  t = 0.95 -h
		  l = 0.95 -w
	  spawnCmus  = myTerminal ++ " -n cmus 'cmus'"
	  findCmus   = resource =? "cmus"
	  manageCmus = customFloating $ W.RationalRect l t w h
		where
		  h = 0.9
		  w = 0.9
		  t = 0.95 -h
		  l = 0.95 -w
