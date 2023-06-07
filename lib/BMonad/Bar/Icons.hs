{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module BMonad.Bar.Icons where

import           BMonad.Config.Types   (Color)

cfi :: Color -> Int -> String -> String
cfi c f i = "<fc=" ++ c ++ "><fn=" ++ show f ++ ">" ++ i ++ "</fn></fc>"

cfi' :: Color -> Int -> String -> String -> String
cfi' c f i t = "<fc=" ++ c ++ "><fn=" ++ show f ++ ">" ++ i ++ "</fn>" ++ t ++ "</fc>"

icoCalendar :: Color -> String
icoCalendar c = cfi c 2 "\xf073"

icoCalendar' :: Color -> String -> String
icoCalendar' c = cfi' c 2 "\xf073"

icoArrowUp :: Color -> String
icoArrowUp c = cfi c 2 "\xf0aa"

icoArrowUp' :: Color -> String -> String
icoArrowUp' c = cfi' c 2 "\xf0aa"

icoArrowDown :: Color -> String
icoArrowDown c = cfi c 2 "\xf0ab"

icoArrowDown' :: Color -> String -> String
icoArrowDown' c = cfi' c 2 "\xf0ab"

icoDesktop :: Color -> String
icoDesktop c = cfi c 2 "\xf108"

icoDesktop' :: Color -> String -> String
icoDesktop' c = cfi' c 2 "\xf108"

icoServer :: Color -> String
icoServer c = cfi c 2 "\xf233"

icoServer' :: Color -> String -> String
icoServer' c = cfi' c 2 "\xf233"

icoFloppy :: Color -> String
icoFloppy c = cfi c 2 "\xf0c7"

icoFloppy' :: Color -> String -> String
icoFloppy' c = cfi' c 2 "\xf0c7"

icoMemory :: Color -> String
icoMemory c = cfi c 2 "\xf85a"

icoMemory' :: Color -> String -> String
icoMemory' c = cfi' c 2 "\xe266" --"\xf85a"

icoCpu :: Color -> String
icoCpu c = cfi c 2 "\xf0ee0"

icoCpu' :: Color -> String -> String
icoCpu' c = cfi' c 2 "\xf0ee0"

icoGit :: Color -> String
icoGit c = cfi c 4 "\xf418" --"\xf1d2"

icoGit' :: Color -> String -> String
icoGit' c = cfi' c 4 "\xf418" --"\xf1d2"
