module Main (main) where

import GrammarGrouch.Reddit.API (getNewestPosts)
import GrammarGrouch.Reddit.Auth (RedditApp (..), requestAccessToken)
import Options.Applicative
  ( ParserInfo,
    briefDesc,
    execParser,
    helper,
    info,
    long,
    progDesc,
    strOption,
    switch,
  )
import Relude

data Options = Options
  { redditClientID :: Text,
    secret :: Text,
    debugMode :: Bool
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
        <*> switch (long "debug")

main :: IO ()
main = do
  Options {..} <- execParser opts
  let redditApp =
        RedditApp
          { clientID = redditClientID,
            clientSecret = secret,
            redirectURI = "http://localhost:8000",
            deviceID = "DO_NOT_TRACK_THIS_DEVICE",
            grantType = "client_credentials",
            debugMode
          }
  creds <- requestAccessToken redditApp
  print creds
  print redditApp
  newestPosts <- getNewestPosts debugMode
  putStrLn $ show newestPosts
