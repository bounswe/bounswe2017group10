{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec.WebDriver
import qualified Test.WebDriver.Capabilities as W
import           Test.WebDriver.Commands.Wait
import           Data.Functor

allBrowsers :: [Capabilities]
allBrowsers = [chromeCaps]

main :: IO ()
main = hspec $
    describe "XKCD Tests" $
      session "for 327" $ using allBrowsers $ do
        it "opens the page" $ runWD $
          openPage "http://localhost:3000"
        it "clicks login" $ runWD $ do
          e <- findElem $ ByXPath "//a[contains(text(), 'Login')]"
          click e
          void $ waitUntil 10 $ findElem $ ByXPath "//h3[@text='Log-in']"
