module BMonad.Games (listTitles) where

import           Control.Applicative
import           Control.Monad

import           System.Directory
import           System.FilePath

import           Data.Ini
import           Data.List           (find)
import           Data.Maybe          (catMaybes)
import           Data.Tree

import           Text.Regex.Posix

import           XMonad

-- |RegEx Context
type NameMatch = (String, String, String, [String])

-- |Group name only of parent directory
nameRx = "^(.*)[_-]v?[0-9].*$"

-- |Run name regex on the given string and return the matched substring
gameTitle :: String -> String
gameTitle s = let (_, _, _, [name]) = s =~ nameRx :: NameMatch in name

-- |Attempt to extract game launcher script from contents
maybeLauncher :: [FilePath] -> Maybe FilePath
maybeLauncher = find (\a -> ".sh" == takeExtension a)

-- |Combine game dir with launcher script from contents
mkEntry :: FilePath -> Maybe FilePath -> Maybe (String, FilePath)
mkEntry a (Just b) = Just (gameTitle $ takeBaseName a, b)
mkEntry a Nothing  = Nothing

-- |Convert game folder into a pair (name, launcher)
processTitle :: FilePath -> IO (Maybe (String, FilePath))
processTitle p = mkEntry p <$> maybeLauncher <$> listAbsDir p

-- |List games in the given directory root it it exists
-- Input path is the folder holding games
-- Rules: Game folder must have a '.sh' launcher to qualify
listTitles :: FilePath -> IO [(String, FilePath)]
listTitles p = do
  exists <- doesDirectoryExist p
  if exists
    then do
       files      <- listSubDirs p
       maybeGames <- sequence $ map processTitle files
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
