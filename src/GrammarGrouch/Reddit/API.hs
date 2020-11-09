module GrammarGrouch.Reddit.API () where

import Control.Lens ((.~), (?~), (^.))
import Data.Aeson (FromJSON (..), eitherDecode)
import GrammarGrouch.Reddit.Auth (RedditCredentials (..))
import GrammarGrouch.Reddit.Internal (requestAsJSON)
import Network.Wreq (FormParam ((:=)), Response, auth, basicAuth, defaults, getWith, header, postWith, responseBody)
import Relude

redditAPIURL :: String
redditAPIURL = "https://reddit.com/api/v1"

requestRedditAPI :: (MonadIO m, FromJSON t) => RedditCredentials -> String -> m t
requestRedditAPI RedditCredentials {accessToken} url =
  requestAsJSON $
    getWith (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken]) url
