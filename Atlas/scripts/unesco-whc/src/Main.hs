{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Database             (insertHeritages)
import           Network.HTTP
import           Parser               (parseHeritages)

url = "http://whc.unesco.org/en/list/xml/"
main :: IO ()
main =
  either (error . show) handleResp =<< simpleHTTP (getRequest url)

handleResp :: Response String -> IO ()
handleResp (Response (2, _, _) reason headers body) =
  insertHeritages =<< parseHeritages body
handleResp _ =
  error $ "Error: non-success response while fetching " <> url
