{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Text     hiding (head)
import           GHC.Generics
import           Network.Wreq

endpoint :: String
endpoint = "http://hn.algolia.com/api/v1/search?tags=comment"

data Comment =
  Comment {_commentText :: Text
          ,_author      :: Text
          ,_storyTitle  :: Text}
  deriving (Eq,Show,Read,Generic)
makeLenses ''Comment

instance FromJSON Comment where
  parseJSON (Object o) = Comment <$> o .: "comment_text"
                                 <*> o .: "author"
                                 <*> o .: "story_title"
  parseJSON _ = mzero

data Hit =
  Hit {_comments :: [Comment]}
  deriving (Eq,Show,Generic)
makeLenses ''Hit

instance FromJSON Hit where
  parseJSON (Object o) = Hit <$> o .: "hits"
  parseJSON _ = mzero

getHits :: IO Hit
getHits =
  do response <- get endpoint
     page <- asJSON response
     return $ page ^. responseBody

main :: IO ()
main =
  do hits <- getHits
     print $
       head $
       view comments hits
