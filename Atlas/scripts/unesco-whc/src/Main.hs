{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Database             (insertHeritages)
import           Network.HTTP
import           Parser               (parseHeritages)
import           Text.Pretty.Simple

url = "http://whc.unesco.org/en/list/xml/"
main :: IO ()
main = do
  readFile "data.xml" >>= parseHeritages >>= insertHeritages
