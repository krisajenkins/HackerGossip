{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Map      (Map, alter)
import qualified Data.Map      as Map
import           Data.Maybe
import           Data.Monoid
-- import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Text     hiding (filter, foldl, head)
import           GHC.Generics
import           Network.Wreq
import           Prelude       hiding (Word, unwords, words)

endpoint :: String
endpoint = "http://hn.algolia.com/api/v1/search?tags=comment&hitsPerPage=200"

type Word = Text
type Words = [Word]

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



frequency :: [Word] -> Map Text Int
frequency ws = foldl reducer Map.empty ws
  where reducer m w = alter updater w m
        updater Nothing = Just 1
        updater (Just n) = Just (n + 1)

allFreqs :: [Comment] -> Map Text Int
allFreqs cs =
  frequency $
  mconcat $
  fmap (words . view commentText) cs

-- findUniqueWords :: Map Text Int -> [Text] -> [Text]
-- findUniqueWords freqs ws = filter pred
--   where

-- wordWeights :: [Comment] ->  Map Text Double
distance :: Map Word Int -> Words -> Words -> Double
distance freqs as bs = weightOf unionWords / weightOf intersectionWords
  where unionWords =
          Set.union (Set.fromList as)
                    (Set.fromList bs)
        intersectionWords =
          Set.intersection (Set.fromList as)
                           (Set.fromList bs)
        weightOf ws =
          fromIntegral $
          sum $
          fmap (\w ->
                  fromMaybe 0 $
                  Map.lookup w freqs) $
          Set.toList ws


-- centerCost :: (a -> a -> Double) -> [a] -> a -> Double
-- centerCost distance points center =
--   sum (fmap distanceSquared points)
--   where distanceSquared x =
--           (distance center x) *
--           (distance center x)

-- chooseCenter :: (a -> a -> Double) -> [a]
-- chooseCenter distance points = _

-- group :: (a -> a -> Double) -> [a] -> [a] -> [[a]]
-- group distance centers points = _

-- kmedoids :: Int -> (a -> a -> Double) -> [a] -> [a]
-- kmedoids = _

stripPopularWords :: Map Word Int -> Words -> Words
stripPopularWords freqs = filter f
  where f w =
          case Map.lookup w freqs of
            Nothing -> True
            Just n -> n < 2

format :: Show a => a -> IO ()
format x =
  putStr $
  (show x) ++
  "\n\n"

main :: IO ()
main =
  do hits <- getHits
     let freq =
           allFreqs $
           view comments hits
     mapM_ (format . unwords . stripPopularWords freq . words . view commentText)
           (view comments hits)
