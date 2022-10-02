module BMonad.Bar.Utils where

import BMonad.Colors

(<~>) :: Colors -> [String] -> [String]
(<~>) c args = args ++ ["--low", color04 c, "--normal", color08 c, "--high", color02 c]

fc :: String -> String -> String
fc color thing = "<fc=" ++ color ++ ">" ++ thing ++ "</fc>"

fn :: (Show a) => a -> String -> String
fn n thing = "<fn=" ++ show n ++ ">" ++ thing ++ "</fn>"

fni :: String -> String
fni = fn 6

fcAqua :: Colors -> String -> String
fcAqua c args = fc (color15 c) args

fcRed :: Colors -> String -> String
fcRed c args = fc (color10 c) args

fcGreen :: Colors -> String -> String
fcGreen c args = fc (color11 c) args

fcYellow :: Colors -> String -> String
fcYellow c args = fc (color12 c) args

fcOrange :: Colors -> String -> String
fcOrange c args = fc (color04 c) args

fcBlue :: Colors -> String -> String
fcBlue c args = fc (color13 c) args

fcGray :: Colors -> String -> String
fcGray c args = fc (color09 c) args

fcBg :: Colors -> String -> String
fcBg c args = fc (colorBack c) args
