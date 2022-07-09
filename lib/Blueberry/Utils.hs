module Blueberry.Utils (listGames, fc, fn, fni) where

import Control.Applicative
import Control.Monad

import System.Directory
import System.FilePath

import Data.Tree
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Ini

import Text.Regex.Posix

import XMonad
import qualified XMonad.Actions.TreeSelect as TS

-----------
-- RegEx --
-----------

-- |Standard RegEx Context
type NameMatch = (String, String, String, [String])

-- |Group name only of parent directory
nameRx = "^(.*)[_-]v?[0-9].*$"

-----------------------
-- Utility Functions --
-----------------------

-- |Run name regex on the given string and return the matched substring
gameName :: String -> String
gameName s = 
  let (_, _, _, [name]) = s =~ nameRx :: NameMatch
   in name

-- |Attempt to extract game launcher script from contents
maybeLauncher :: [FilePath] -> Maybe FilePath
maybeLauncher = find (\a -> ".sh" == takeExtension a)

-- |Combine game dir with launcher file if any
mkEntry :: FilePath -> Maybe FilePath -> Maybe (String, FilePath)
mkEntry a (Just b)  = Just (gameName $ takeBaseName a, b)
mkEntry a Nothing   = Nothing

-- |Convert game folder into a pair (name, launcher)
processGame :: FilePath -> IO (Maybe (String, FilePath))
processGame p = mkEntry p <$> maybeLauncher <$> listAbsDir p

-- |List games in the given directory if it exists
-- Input path is the folder holding games
-- Rules: Game folders must have a '.sh' launcher to qualify
listGames :: FilePath -> IO [(String, FilePath)]
listGames p = do
  exists <- doesDirectoryExist p
  if exists
    then do
      files      <- listSubDirs p
      maybeGames <- sequence $ map processGame files
      pure $ catMaybes maybeGames
    else pure []

-- |List directory contents as absolute paths
listAbsDir :: FilePath -> IO [FilePath]
listAbsDir d = listDirectory d >>= mapM (canonicalizePath . (d </>))

-- |List all sub-directories in some parent dir
listSubDirs :: FilePath -> IO [FilePath]
listSubDirs d = do
  cx <- listAbsDir d
  filterM (doesDirectoryExist . (d </>)) cx

------------------
-- Xmobar Utils --
------------------

-- |Colorize xmobar item
fc :: String -> String -> String
fc color thing = "<fc=" ++ color ++ ">" ++ thing ++ "</fc>"

fn :: (Show a) => a -> String -> String
fn n thing = "<fn=" ++ show n ++ ">" ++ thing ++ "</fn>"

fni :: String -> String
fni = fn 6
