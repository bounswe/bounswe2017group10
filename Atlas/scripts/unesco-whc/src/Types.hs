module Types where

import qualified Data.Text as T

data Heritage = Heritage
  { _site      :: T.Text
  , _shortDesc :: T.Text
  , _category  :: T.Text
  , _image     :: T.Text
  , _states    :: [T.Text]
  } deriving Show

