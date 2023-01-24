{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module BMonad.Bar.Icons where

import           BMonad.Config.Types   (Color)
import           BMonad.Image.NerdFont (nfFaArrowCircleDown, nfFaArrowCircleUp,
                                        nfFaCalendar, nfFaDesktop, nfFaSave,
                                        nfFaServer, nfMdCpu64Bit,
                                        nfMdiCurrencyBtc, nfMdiCurrencyEth,
                                        nfMdiMemory)

-- |Index of FontAwesome (Glyphs) in additional-icons setting.
fontAwesome :: Int
fontAwesome = 2

colorAndFont :: (Show a) => Color -> a -> String -> String
colorAndFont c f i = "<fc=" ++ c ++ ">" ++ "<fn=" ++ show f ++ ">" ++ i ++ "</fn></fc>"

colorAndFont' :: (Show a) => Color -> a -> String -> String -> String
colorAndFont' c f i t = "<fc=" ++ c ++ ">" ++ "<fn=" ++ show f ++ ">" ++ i ++ "</fn>" ++ t ++ "</fc>"

icoCalendar :: Color -> String
icoCalendar c = colorAndFont c fontAwesome nfFaCalendar

icoCalendar' :: Color -> String -> String
icoCalendar' c = colorAndFont' c fontAwesome nfFaCalendar

icoBitcoin :: Color -> String
icoBitcoin c = colorAndFont c fontAwesome nfMdiCurrencyBtc

icoBitcoin' :: Color -> String -> String
icoBitcoin' c = colorAndFont' c fontAwesome nfMdiCurrencyBtc

icoEthereum :: Color -> String
icoEthereum c = colorAndFont c fontAwesome nfMdiCurrencyEth

icoEthereum' :: Color -> String -> String
icoEthereum' c = colorAndFont' c fontAwesome nfMdiCurrencyEth

icoArrowUp :: Color -> String
icoArrowUp c = colorAndFont c fontAwesome nfFaArrowCircleUp

icoArrowUp' :: Color -> String -> String
icoArrowUp' c = colorAndFont' c fontAwesome nfFaArrowCircleUp

icoArrowDown :: Color -> String
icoArrowDown c = colorAndFont c fontAwesome nfFaArrowCircleDown

icoArrowDown' :: Color -> String -> String
icoArrowDown' c = colorAndFont' c fontAwesome nfFaArrowCircleDown

icoDesktop :: Color -> String
icoDesktop c = colorAndFont c fontAwesome nfFaDesktop

icoDesktop' :: Color -> String -> String
icoDesktop' c = colorAndFont' c fontAwesome nfFaDesktop

icoServer :: Color -> String
icoServer c = colorAndFont c fontAwesome nfFaServer

icoServer' :: Color -> String -> String
icoServer' c = colorAndFont' c fontAwesome nfFaServer

icoFloppy :: Color -> String
icoFloppy c = colorAndFont c fontAwesome nfFaSave

icoFloppy' :: Color -> String -> String
icoFloppy' c = colorAndFont' c fontAwesome nfFaSave

icoMemory :: Color -> String
icoMemory c = colorAndFont c fontAwesome nfMdiMemory

icoMemory' :: Color -> String -> String
icoMemory' c = colorAndFont' c fontAwesome nfMdiMemory

icoCpu :: Color -> String
icoCpu c = colorAndFont c fontAwesome nfMdCpu64Bit

icoCpu' :: Color -> String -> String
icoCpu' c = colorAndFont' c fontAwesome nfMdCpu64Bit
