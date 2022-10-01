module BMonad.Utils where

import           BMonad.Variables (myWorkspaceIndices)

import           Data.Char        as Char
import qualified Data.Map         as M
import           Data.Maybe       (fromJust)

capitalized :: String -> String
capitalized []          = []
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices
