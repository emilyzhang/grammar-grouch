module GrammarGrouch.Reddit.API (getNewestPosts) where

import Control.Lens ((.~))
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.List
import GrammarGrouch.Reddit.Auth (RedditCredentials (..))
import GrammarGrouch.Reddit.Internal (requestAsJSON)
import Network.Wreq (defaults, getWith, header)
import Relude

redditAPIURL :: String
redditAPIURL = "https://reddit.com/api/v1"

newestPostsURL :: String
newestPostsURL = "https://www.reddit.com/r/all/new/.json?t=all&limit=200"

testNewestPostsURL :: String
testNewestPostsURL = "https://www.reddit.com/r/testingground4bots/new/.json?t=all&limit=10"

getRedditAPI :: (MonadIO m, FromJSON t) => RedditCredentials -> String -> m t
getRedditAPI RedditCredentials {accessToken} url =
  requestAsJSON $
    getWith (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken]) url

getNewestPosts :: (MonadIO m) => Bool -> m NewestPostsResponse
getNewestPosts debugMode = requestAsJSON $ getWith defaults $ if debugMode then testNewestPostsURL else newestPostsURL

-- steps left:
-- 1. parse through all newest posts
-- 2. check for should of/could of/would of
-- 3. reply to posts
-- how do you filter and return?

hasGrammarError :: [Char] -> T3Data -> Bool
hasGrammarError text T3Data {..} = text `isInfixOf` toString selftext

-- TODO: filter posts for grammar errors
-- filterGrammarErrorPosts :: NewestPostsResponse -> [T3]
-- filterGrammarErrorPosts NewestPostsResponse {..} =
-- filter hasGrammarError ["should of", "would of", "could of"] (children listingData)

data NewestPostsResponse = NewestPostsResponse
  {listingData :: ListingData}
  deriving (Show, Generic)

data ListingData = ListingData
  { children :: [T3]
  }
  deriving (Show, Generic)

instance FromJSON ListingData

data T3 = T3
  { kind :: Text,
    t3Data :: T3Data
  }
  deriving (Show, Generic)

instance FromJSON T3 where
  parseJSON = withObject "newest_posts_response" $ \r -> do
    t3Data <- r .: "data"
    kind <- r .: "kind"
    return T3 {..}

data T3Data = T3Data
  { id :: Text,
    subreddit :: Text,
    hidden :: Bool,
    selftext :: Text,
    permalink :: Text,
    author :: Text,
    title :: Text,
    num_comments :: Integer,
    subreddit_name_prefixed :: Text,
    subreddit_subscribers :: Integer,
    over_18 :: Bool,
    locked :: Bool
  }
  deriving (Show, Generic)

instance FromJSON T3Data

instance FromJSON NewestPostsResponse where
  parseJSON = withObject "newest_posts_response" $ \r -> do
    listingData <- r .: "data"
    return NewestPostsResponse {..}
