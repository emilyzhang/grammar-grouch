module Main (main) where

import Control.Lens ((.~), (?~), (^.))
import Data.Aeson (FromJSON (..), eitherDecode)
import Network.Wreq (FormParam ((:=)), Response, auth, basicAuth, defaults, header, postWith, responseBody)
import Options.Applicative
  ( ParserInfo,
    briefDesc,
    execParser,
    helper,
    info,
    long,
    progDesc,
    strOption,
  )
import Relude

-- redditAPIURL :: String
-- redditAPIURL = "https://reddit.com/api/v1"

redditTokenURL :: String
redditTokenURL = "https://www.reddit.com/api/v1/access_token"

data Options = Options
  { redditClientID :: Text,
    secret :: Text
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "grammar grouch bot")
  where
    options =
      Options
        <$> strOption (long "client_id")
        <*> strOption (long "client_secret")

main :: IO ()
main = do
  Options {..} <- execParser opts
  let redditApp =
        RedditApp
          { clientID = redditClientID,
            clientSecret = secret,
            redirectURI = "http://localhost:8000",
            deviceID = "DO_NOT_TRACK_THIS_DEVICE",
            grantType = "client_credentials"
          }
  creds <- requestAccessToken redditApp
  putStrLn $ show creds

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
