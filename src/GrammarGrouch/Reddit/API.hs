module GrammarGrouch.Reddit.API (getNewestPosts) where

import Control.Lens ((.~))
import Data.Aeson (FromJSON (..), withObject, (.:))
import GrammarGrouch.Reddit.Auth (RedditCredentials (..))
import GrammarGrouch.Reddit.Internal (requestAsJSON)
import Network.Wreq (defaults, getWith, header)
import Relude

redditAPIURL :: String
redditAPIURL = "https://reddit.com/api/v1"

newestPostsURL :: String
newestPostsURL = "https://www.reddit.com/r/all/new/.json?t=all&limit=200"

getRedditAPI :: (MonadIO m, FromJSON t) => RedditCredentials -> String -> m t
getRedditAPI RedditCredentials {accessToken} url =
  requestAsJSON $
    getWith (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken]) url

getNewestPosts :: (MonadIO m) => m NewestPostsResponse
getNewestPosts = do
  newestPosts <- requestAsJSON $ getWith (defaults) newestPostsURL
  return newestPosts

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
