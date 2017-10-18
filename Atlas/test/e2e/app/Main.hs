{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec.WebDriver
import qualified Test.WebDriver.Capabilities  as W

allBrowsers :: [Capabilities]
allBrowsers =
  let binary = "/Users/yigitozkavci/Documents/Programming/PythonWorkspace/bounswe2017group10/Atlas/test/e2e/"
  in
    [ chromeCaps { W.browser = W.chrome { W.chromeBinary = Just binary, W.chromeOptions = ["--kiosk"] } }
    ]

main :: IO ()
main = hspec $
    describe "XKCD Tests" $ do
      session "for 327" $ using allBrowsers $ do
        it "opens the page" $ runWD $
          openPage "http://www.xkcd.com/327/"
        it "checks hover text" $ runWD $ do
          e <- findElem $ ByCSS "div#comic > img"
          e `shouldBeTag` "img"
          e `shouldHaveAttr` ("title", "Her daughter is named Help I'm trapped in a driver's license factory.")
