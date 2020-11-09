module GrammarGrouch.Reddit.Auth
  ( RedditApp (..),
    RedditTokenResponse,
    requestAccessToken,
    requestAsJSON,
  )
where

import Control.Lens ((.~), (?~), (^.))
import Data.Aeson (FromJSON (..), eitherDecode)
import Network.Wreq (FormParam ((:=)), Response, auth, basicAuth, defaults, header, postWith, responseBody)
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
  { bearerToken :: Text,
    token_type :: Text,
    expires_in :: Int,
    scope :: Text
  }
  deriving (Generic, Show)

-- | requestAccessToken makes requests for API tokens.
requestAccessToken :: (MonadIO m) => RedditApp -> m RedditTokenResponse
requestAccessToken RedditApp {..} =
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

requestAsJSON :: (MonadIO m, FromJSON t) => IO (Response LByteString) -> m t
requestAsJSON req = do
  res <- liftIO req
  let body = res ^. responseBody
  case eitherDecode body of
    Right v -> return v
    Left e -> do
      print res
      error $ "JSONError: " <> toText e
