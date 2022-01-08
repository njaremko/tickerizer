module Pre
  ( module Relude,
    runM,
    sendIO,
    sendM,
    ask,
    runReader,
    throwIO,
    try,
    Reader,
    ReaderC,
    Has,
    Lift,
    LiftC,
    Port,
    BaseUrl,
    SlackClientId,
    SlackClientSecret,
    openPort,
    openBaseUrl,
    openSlackClientId,
    openSlackClientSecret,
    Web,
    WebConcrete,
    AppState(..),
    SlackPayload(..),
    SlackResponse(..)
  )
where

import Control.Carrier.Lift (Has, Lift, LiftC, runM, sendIO, sendM)
import Control.Carrier.Reader (Reader, ReaderC, ask, runReader)
import Control.Exception (throwIO, try)
import Relude hiding (Reader, ask, id, runReader)
import System.Envy (FromEnv, ToEnv)
import qualified System.Envy as Envy
import Data.Trie (Trie)
import Web.FormUrlEncoded (FromForm)
import Data.Aeson

newtype Port = Port Int
  deriving newtype (Envy.Var, Show)

openPort :: Port -> Int
openPort (Port port) = port

instance FromEnv Port where
  fromEnv _ = Port <$> Envy.env "PORT"

instance ToEnv Port where
  toEnv port = Envy.makeEnv [ "PORT" Envy..= port ]

newtype BaseUrl = BaseUrl Text
  deriving newtype (Show, Semigroup, Monoid)

openBaseUrl :: BaseUrl -> Text
openBaseUrl (BaseUrl baseUrl) = baseUrl

instance FromEnv BaseUrl where
  fromEnv _ = BaseUrl 
    <$> Envy.env "BASE_URL" 

instance ToEnv BaseUrl where
  toEnv (BaseUrl baseUrl) =
    Envy.makeEnv
      [ "BASE_URL" Envy..= baseUrl
      ]

newtype SlackClientId = SlackClientId Text
  deriving newtype (Show, Semigroup, Monoid)

openSlackClientId :: SlackClientId -> Text
openSlackClientId (SlackClientId clientId) = clientId

instance FromEnv SlackClientId where
  fromEnv _ = SlackClientId 
    <$> Envy.env "SLACK_CLIENT_ID" 

instance ToEnv SlackClientId where
  toEnv (SlackClientId clientId) =
    Envy.makeEnv
      [ "SLACK_CLIENT_ID" Envy..= clientId
      ]

newtype SlackClientSecret = SlackClientSecret Text
  deriving newtype (Show, Semigroup, Monoid)

openSlackClientSecret :: SlackClientSecret -> Text
openSlackClientSecret (SlackClientSecret clientSecret) = clientSecret

instance FromEnv SlackClientSecret where
  fromEnv _ = SlackClientSecret 
    <$> Envy.env "SLACK_CLIENT_SECRET" 

instance ToEnv SlackClientSecret where
  toEnv (SlackClientSecret clientSecret) =
    Envy.makeEnv
      [ "SLACK_CLIENT_SECRET" Envy..= clientSecret
      ]

type Web sig m =
  ( Has (Reader AppState) sig m,
    Has (Lift IO) sig m
  )

type WebConcrete = 
  (ReaderC AppState 
  (LiftC IO))

data AppState = AppState
  { baseUrl :: BaseUrl,
    port :: Port,
    slackClientId :: SlackClientId,
    slackClientSecret :: SlackClientSecret,
    tickerTrie :: Trie ()
  }

data SlackPayload = SlackPayload {
  token :: Maybe Text,
  team_id :: Maybe Text,
  team_domain :: Maybe Text,
  enterprise_id :: Maybe Text,
  enterprise_name :: Maybe Text,
  channel_id :: Maybe Text,
  channel_name :: Maybe Text,
  user_id :: Maybe Text,
  user_name :: Maybe Text,
  command :: Maybe Text,
  text :: Text,
  response_url :: Maybe Text,
  trigger_id :: Maybe Text,
  api_app_id :: Maybe Text
} deriving stock (Eq, Show, Generic)

instance FromForm SlackPayload

data SlackResponse = SlackResponse {
  text :: Text,
  response_type :: Text
} deriving stock (Eq, Show, Generic)

instance FromJSON SlackResponse
instance ToJSON SlackResponse where
  toEncoding = genericToEncoding defaultOptions 