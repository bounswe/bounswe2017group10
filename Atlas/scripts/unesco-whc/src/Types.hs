module Types where

import qualified Data.Text as T

data Heritage = Heritage
  { _site      :: T.Text
  , _shortDesc :: T.Text
  , _category  :: T.Text
  , _image     :: T.Text
  , _lat       :: T.Text
  , _long      :: T.Text
  , _states    :: [T.Text]
  } deriving (Show)
  
instance Eq Heritage where
  Heritage { _image = image1 } == Heritage { _image = image2 } = image1 == image2

instance Ord Heritage where
  Heritage { _image = image1 } `compare` Heritage { _image = image2 } = image1 `compare` image2
