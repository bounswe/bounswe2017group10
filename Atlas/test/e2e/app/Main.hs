{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec.WebDriver
import qualified Test.WebDriver.Capabilities as W
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait
import           Data.Functor
import           Control.Concurrent
import           Control.Monad.IO.Class

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
          _ <- waitUntil 10 $ findElem $ ByXPath "//*[contains(text(), 'Log-in')]"
          loginInput <- findElem $ ByXPath "//input[@name='username']"
          passInput <- findElem $ ByXPath "//input[@name='password']"
          sendKeys "yigitozkavci" loginInput
          sendKeys "220513Yigit" passInput
          click =<< waitUntil 10 (findElem (ByXPath "//button[contains(text(), 'Log-in')]"))
          liftIO $ threadDelay 10000
