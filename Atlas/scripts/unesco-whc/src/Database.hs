{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Database where

import qualified Data.Text                  as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Types

insertHeritages :: [Heritage] -> IO ()
insertHeritages (take 50 -> heritages) = do
  now <- getCurrentTime
  db_name <- getEnv "ATLAS_DB_NAME"
  db_user <- getEnv "ATLAS_DB_USER"
  db_pass <- getEnv "ATLAS_DB_PASSWORD"
  conn <- connectPostgreSQL $ "host='localhost' "
                            <> "port=5432 "
                            <> "dbname='" <> db_name <> "' "
                            <> "user='" <> db_user <> "' "
                            <> "password='" <> db_pass <> "'"
  executeMany conn
    "INSERT INTO atlas_cultural_heritage(title, description, user_id, continent, country, city, public_accessibility, created_time, updated_time) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (map (toTuple now) heritages)
  return ()
    where
      toTuple :: UTCTime -> Heritage -> (T.Text, T.Text, Int, String, String, String, Bool, UTCTime, UTCTime)
      toTuple now Heritage {..} =
        (_site, _shortDesc, 1, "", "", "", True, now, now) -- 1 is the user id of [Yigit Ozkavci]
