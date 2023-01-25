{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module BMonad.Bar.Icons where

import           BMonad.Config.Types   (Color)

colorAndFont :: Color -> String -> String
colorAndFont c i = "<fc=" ++ c ++ ">" ++ i ++ "</fc>"

colorAndFont' :: Color -> String -> String -> String
colorAndFont' c i t = "<fc=" ++ c ++ ">" ++ i ++ t ++ "</fc>"

icoCalendar :: Color -> String
icoCalendar c = colorAndFont c "\xf073"

icoCalendar' :: Color -> String -> String
icoCalendar' c = colorAndFont' c "\xf073"

icoBitcoin :: Color -> String
icoBitcoin c = colorAndFont c "\xf6ab"

icoBitcoin' :: Color -> String -> String
icoBitcoin' c = colorAndFont' c "\xf6ab"

icoEthereum :: Color -> String
icoEthereum c = colorAndFont c "\xfcb9"

icoEthereum' :: Color -> String -> String
icoEthereum' c = colorAndFont' c "\xfcb9"

icoArrowUp :: Color -> String
icoArrowUp c = colorAndFont c "\xf0aa"

icoArrowUp' :: Color -> String -> String
icoArrowUp' c = colorAndFont' c "\xf0aa"

icoArrowDown :: Color -> String
icoArrowDown c = colorAndFont c "\xf0ab"

icoArrowDown' :: Color -> String -> String
icoArrowDown' c = colorAndFont' c "\xf0ab"

icoDesktop :: Color -> String
icoDesktop c = colorAndFont c "\xf108"

icoDesktop' :: Color -> String -> String
icoDesktop' c = colorAndFont' c "\xf108"

icoServer :: Color -> String
icoServer c = colorAndFont c "\xf233"

icoServer' :: Color -> String -> String
icoServer' c = colorAndFont' c "\xf233"

icoFloppy :: Color -> String
icoFloppy c = colorAndFont c "\xf0c7"

icoFloppy' :: Color -> String -> String
icoFloppy' c = colorAndFont' c "\xf0c7"

icoMemory :: Color -> String
icoMemory c = colorAndFont c "\xf85a"

icoMemory' :: Color -> String -> String
icoMemory' c = colorAndFont' c "\xf85a"

icoCpu :: Color -> String
icoCpu c = colorAndFont c "\xf0ee0"

icoCpu' :: Color -> String -> String
icoCpu' c = colorAndFont' c "\xf0ee0"
