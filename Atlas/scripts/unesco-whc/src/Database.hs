{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Types
import Database.PostgreSQL.Simple
import Control.Arrow ((&&&))
import Data.Time.Clock
import qualified Data.Text as T

insertHeritages :: [Heritage] -> IO ()
insertHeritages (take 2 -> heritages) = do
  now <- getCurrentTime
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='atlas'"
  executeMany conn
    "INSERT INTO atlas_cultural_heritage(title, description, user_id, continent, country, city, public_accessibility, created_time, updated_time) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (map (toTuple now) heritages)
  return ()
    where
      toTuple :: UTCTime -> Heritage -> (T.Text, T.Text, Int, String, String, String, Bool, UTCTime, UTCTime)
      toTuple now Heritage {..} =
        (_site, _shortDesc, 1, "", "", "", True, now, now) -- 1 is the user id of [Yigit Ozkavci]
