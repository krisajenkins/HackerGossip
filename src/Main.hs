{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Map        (Map, alter)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set        as Set
import           Data.Text       hiding (filter, foldl, head)
import           GHC.Generics
import           Network.Wreq
import           Prelude         hiding (Word, unwords, words)

endpoint :: String
endpoint = "http://hn.algolia.com/api/v1/search?tags=comment&hitsPerPage=50"

type Word = Text
type Words = [Word]

data Comment =
  Comment {_commentText :: Text
          ,_author      :: Text
          ,_storyTitle  :: Text}
  deriving (Eq,Show,Read,Generic)
makeLenses ''Comment

instance ToJSON Comment
instance FromJSON Comment where
  parseJSON (Object o) = Comment <$> o .: "comment_text"
                                 <*> o .: "author"
                                 <*> o .: "story_title"
  parseJSON _ = mzero

getComments :: IO [Comment]
getComments =
  do response <- get endpoint
     return $ response ^.. responseBody . key "hits" . _Array . traverse . _JSON

frequency :: [Word] -> Map Text Int
frequency = foldl reducer Map.empty
  where reducer = flip $ alter updater
        updater = Just . (+ 1) . fromMaybe 0

allFreqs :: [Comment] -> Map Text Int
allFreqs =
  frequency .
  mconcat .
  fmap (words . view commentText)

findDefault :: Ord k => a -> Map k a -> k -> a
findDefault = flip . Map.findWithDefault

distance :: Map Word Int -> Words -> Words -> Double
distance freqs as bs = weightOf unionWords / weightOf intersectionWords
  where unionWords =
          Set.union (Set.fromList as)
                    (Set.fromList bs)
        intersectionWords =
          Set.intersection (Set.fromList as)
                           (Set.fromList bs)
        weightOf =
          fromIntegral .
          sum .
          fmap (findDefault 0 freqs) .
          Set.toList

stripPopularWords :: Map Word Int -> Words -> Words
stripPopularWords freqs =
  filter ((< 2) .
          findDefault 0 freqs)

format :: Show a => a -> IO ()
format x = putStrLn $ show x ++ "\n"

main :: IO ()
main =
  do comments <- getComments
     mapM_ (format .
            unwords .
            stripPopularWords (allFreqs comments) .
            words . view commentText)
           comments
