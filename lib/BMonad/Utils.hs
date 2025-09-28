{-# OPTIONS_GHC -Wno-type-defaults #-}
module BMonad.Utils (
  resolve,
  listAbsDir,
  checkExecutable,
  capitalized,
  lowerCase,
  hexToRgb,
  countScreens,
  countWindows
) where

import qualified Data.Char             as Char
import           Data.Colour.SRGB      (RGB (RGB), sRGB24read, toSRGB24)
import           Data.Word             (Word8)

import           Graphics.X11.Xinerama (getScreenInfo)

import           System.Directory      (Permissions (executable),
                                        canonicalizePath, getPermissions,
                                        listDirectory, makeAbsolute)
import           System.FilePath       ((</>))

import           XMonad                (X, closeDisplay, gets, openDisplay,
                                        windowset)
import qualified XMonad.StackSet       as W

-- |Resolve some relative path (i.e. ~/.)
resolve :: String -> FilePath -> IO FilePath
resolve ('~':'/':rest) home = return $ home </> rest
resolve path _              = makeAbsolute path

-- |List child directories with canonicalized path.
listAbsDir :: FilePath -> IO [FilePath]
listAbsDir d = listDirectory d >>= mapM (canonicalizePath . (d </>))

-- |Check if the optional file is executable.
-- A non-executable file will resolve to nothing.
checkExecutable :: Maybe FilePath -> IO (Maybe FilePath)
checkExecutable Nothing  = return Nothing
checkExecutable (Just f) = do
  perm <- getPermissions f
  return $ if executable perm
             then Just f
             else Nothing

-- |Capitalize a given string
capitalized :: String -> String
capitalized []    = []
capitalized (h:t) = Char.toUpper h : map Char.toLower t

-- |All characters to lower case
lowerCase :: String -> String
lowerCase = map Char.toLower

-- |Convert hex color code to RGB
hexToRgb :: String -> (Word8, Word8, Word8)
hexToRgb hex = extr $ toSRGB24 $ sRGB24read hex
  where extr (RGB r g b) = (r, g, b)

-- |Count the number of connected monitors/screens.
-- Not used anymore as I've moved to an ultra wide monitor
countScreens :: IO Int
countScreens = do
  screens <- do
    dpy   <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  pure $ length screens

-- |Count windows
countWindows :: X (Maybe String)
countWindows = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current. windowset
