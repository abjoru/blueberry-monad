module BMonad.Utils where

import Data.Char as Char

capitalized :: String -> String
capitalized [] = []
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
