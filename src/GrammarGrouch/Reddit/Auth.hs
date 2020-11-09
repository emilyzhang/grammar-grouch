module GrammarGrouch.Reddit.Auth
  ( RedditApp (..),
    RedditCredentials (..),
    RedditTokenResponse,
    requestAccessToken,
  )
where

import Control.Lens ((.~), (?~))
import Data.Aeson (FromJSON (..))
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import GrammarGrouch.Reddit.Internal (requestAsJSON)
import Network.Wreq (FormParam ((:=)), auth, basicAuth, defaults, header, postWith)
import Relude

redditTokenURL :: String
redditTokenURL = "https://www.reddit.com/api/v1/access_token"

data RedditApp = RedditApp
  { -- | The application client ID.
    clientID :: Text,
    -- | The application client secret.
    clientSecret :: Text,
    -- | The URI of the application, for Reddit to redirect back to after
    -- authorization.
    redirectURI :: Text,
    grantType :: Text,
    deviceID :: Text
  }
  deriving (Show)

data RedditTokenResponse = RedditTokenResponse
  { access_token :: Text,
    token_type :: Text,
    expires_in :: Int,
    scope :: Text
  }
  deriving (Generic, Show)

instance FromJSON RedditTokenResponse

data RedditCredentials = RedditCredentials
  { accessToken :: Text,
    expiration :: UTCTime,
    scope :: Text
  }
  deriving (Generic, Show)

-- | requestAccessToken makes requests for API tokens.
requestAccessToken :: (MonadIO m) => RedditApp -> m RedditCredentials
requestAccessToken RedditApp {..} = do
  RedditTokenResponse {..} <-
    requestAsJSON $
      postWith
        ( defaults
            & header "User-Agent" .~ ["script:grammar-grouch-bot:v0 (by grammar-grouch-bot)"]
            & auth
              ?~ basicAuth
                (encodeUtf8 clientID)
                (encodeUtf8 clientSecret)
        )
        redditTokenURL
        [ "grant_type" := grantType,
          "device_id" := deviceID
        ]
  expiration <- expirationToDeadline expires_in
  return $
    RedditCredentials
      { accessToken = access_token,
        ..
      }

-- | expirationToDeadline converts an integer expiration (in seconds) to a
-- deadline by adding it to the current time.
expirationToDeadline :: (MonadIO m) => Int -> m UTCTime
expirationToDeadline expiration = do
  now <- liftIO getCurrentTime
  return $ addUTCTime (fromInteger $ toInteger expiration :: NominalDiffTime) now
