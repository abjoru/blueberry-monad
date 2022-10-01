{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ApplicationsSpec (spec) where

import BMonad.Application

spec :: Spec
spec = parallel $ describe "Applications" $ do
  it "loads application config" $ do
    data <- loadApplications
    return $ putStrLn $ show data
