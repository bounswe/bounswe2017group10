{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Network.HTTP
import           Text.HandsomeSoup    (css, parseHtml)
import           Text.XML.HXT.Core
import           Text.XML.HXT.Core
import Data.Monoid ((<>))

url = "http://whc.unesco.org/en/list/xml/"
main :: IO ()
main =
  either (error . show) handleResp =<< simpleHTTP (getRequest url)

handleResp :: Response String -> IO ()
handleResp (Response (2, _, _) reason headers body) =
  insertHeritages =<< parseHeritages body
handleResp _ =
  error $ "Error: non-success response while fetching " <> url

data Heritage = Heritage
  { _site       :: T.Text
  , _shortDesc :: T.Text
  , _category   :: T.Text
  , _image      :: T.Text
  , _states :: [T.Text]
  } deriving Show

parseHeritages :: String -> IO [Heritage]
parseHeritages body =
  runX $ parseHtml body >>> css "row" >>> parseHeritageXml
  where
    parseHeritageXml :: ArrowXml a => a XmlTree Heritage
    parseHeritageXml = proc x -> do
      _site <- onNode "site" -< x
      _shortDesc <- onNode "short_description" -< x
      _category <- onNode "category" -< x
      _image <- onNode "image_url" -< x
      _states <- T.split (== ',') ^<< onNode "states" -< x
      returnA -< Heritage {..}

    onNode :: ArrowXml a => String -> a XmlTree T.Text
    onNode name =
      css name >>> getChildren >>> isText >>> getText >>^ T.pack

insertHeritages :: [Heritage] -> IO ()
insertHeritages _ = return ()
