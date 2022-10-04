module BMonad.Bar (bmobarPP, mkMobars) where

import           XMonad
import           XMonad.Hooks.DynamicLog (PP (..), shorten, wrap, xmobarColor,
                                          xmobarPP)
import           XMonad.Util.Run         (hPutStrLn, spawnPipe)

import           BMonad.Theme
import           BMonad.Utils            (countScreens)

import           GHC.IO.Handle


import           System.Directory        (getHomeDirectory)
import           System.FilePath         (FilePath (..), (</>))

-- Spawn n blueberry-mobars
spawnBars :: FilePath -> Int -> IO [Handle]
spawnBars b 0 = (: []) <$> spawnPipe (b ++ " single") -- blueberry-mobar single
spawnBars b 1 = (: []) <$> spawnPipe (b ++ " single") -- blueberry-mobar single
spawnBars b 2 = mapM (mkBar b) [(0, "primary"), (1, "secondary")]
spawnBars b 3 = mapM (mkBar b) [(0, "primary"), (1, "secondary"), (2, "other")]
spawnBars b 4 = mapM (mkBar b) [(0, "primary"), (1, "secondary"), (2, "other"), (3, "secondary")]
spawnBars b n = mapM (mkBar b) $ zip (take n [0..]) (["primary", "secondary"] ++ replicate (n - 2) "other")
  
mkBar :: FilePath -> (Int, String) -> IO Handle
mkBar b (i, m) = spawnPipe $ b ++ " -x " ++ show i ++ " " ++ m

mkMobars :: IO [Handle]
mkMobars = do
  home <- getHomeDirectory
  bars <- countScreens >>= spawnBars (home </> ".local" </> "bin" </> "blueberry-mobar")
  return bars

bmobarPP :: BMonadTheme -> X (Maybe String) -> [Handle] -> PP
bmobarPP t wCount bars =
  let cCur   = currentColor t
      cVis   = visibleColor t
      cHid   = hiddenColor t
      cHidN  = hiddenNoWinColor t
      cSep   = sepColor t
      cTitle = titleColor t
      cUrg   = urgentColor t
   in xmobarPP
     { ppOutput = outputBars bars
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
       where outputBars hs s = mapM_ (`hPutStrLn` s) hs
