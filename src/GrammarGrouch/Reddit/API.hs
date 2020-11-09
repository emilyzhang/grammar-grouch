module GrammarGrouch.Reddit.API () where

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

redditAPIURL :: String
redditAPIURL = "https://reddit.com/api/v1"
