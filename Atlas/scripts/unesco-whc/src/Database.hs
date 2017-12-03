{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Database where

import qualified Data.ByteString.Char8      as BS8
import           Data.Monoid
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           System.Environment         (getEnv)
import           Types
import Data.Foldable
import Control.Monad

insertHeritages :: Set.Set Heritage -> IO ()
insertHeritages (take 50 . Set.toList -> heritages) = do
  now <- getCurrentTime
  db_name <- getEnv "ATLAS_DB_NAME"
  db_user <- getEnv "ATLAS_DB_USER"
  db_pass <- getEnv "ATLAS_DB_PASSWORD"
  conn <- connectPostgreSQL $ "host='localhost' "
                            <> "port=5432 "
                            <> "dbname='" <> BS8.pack db_name <> "' "
                            <> "user='" <> BS8.pack db_user <> "' "
                            <> "password='" <> BS8.pack db_pass <> "'"
  forM_ heritages $ \h -> do
    (xs :: [Only Int]) <- query conn ("select id from atlas_cultural_heritage where title = ?") (Only (_site h))
    when (length xs == 0) $
      void $ do
        execute conn
          "INSERT INTO atlas_cultural_heritage(title, description, user_id, continent, country, city, public_accessibility, created_time, updated_time, favorited_amount) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          (toCHTuple now h)
        [Only id'] <- query conn ("select id from atlas_cultural_heritage where title = ?") (Only (_site h))
        execute conn
          "UPDATE atlas_cultural_heritage SET latitude = ?, longitude = ? WHERE id = ?"
          (toUpdateLocTuple h id')
        execute conn
          "INSERT INTO atlas_image_media_item(url, created_time, updated_time, cultural_heritage_item_id, main) VALUES (?, ?, ?, ?, ?)"
          (toImgTuple now h id')
        print (id' :: Int)
    where
      toCHTuple :: UTCTime -> Heritage -> (T.Text, T.Text, Int, String, String, String, Bool, UTCTime, UTCTime, Int)
      toCHTuple now Heritage {..} =
        (_site, _shortDesc, 1, "", "", "", True, now, now, 0) -- 1 is the user id of [Yigit Ozkavci]

      toImgTuple :: UTCTime -> Heritage -> Int -> (T.Text, UTCTime, UTCTime, Int, Bool)
      toImgTuple now Heritage {..} chId =
        (_image, now, now, chId, True) -- 1 is the user id of [Yigit Ozkavci]

      toUpdateLocTuple :: Heritage -> Int -> (T.Text, T.Text, Int)
      toUpdateLocTuple Heritage {..} chId = (_lat, _long, chId)
