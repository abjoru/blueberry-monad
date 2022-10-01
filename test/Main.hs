module Main where

import qualified Spec
import Test.Hspec.Runner

main :: IO ()
main = do
  hspec Spec.spec
