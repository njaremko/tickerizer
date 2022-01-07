{- |
Copyright: (c) 2022 Nathan Jaremko
SPDX-License-Identifier: MIT
Maintainer: Nathan Jaremko <njaremko@wealthsimple.com>

See README for more info
-}

module Tickerizer
    ( runServer, AppState(..)
    ) where

import Pre
import Servant
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Servant.HTML.Lucid ( HTML )
import Lucid (Html, body_, p_, a_, With (with), href_)
import Web.FormUrlEncoded (FromForm)
import Data.Aeson (FromJSON, ToJSON, genericToEncoding, defaultOptions, toEncoding)

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
    tickerTrie :: Trie ()
  }

type HealthApi = "health" :> "alive" :> Get '[JSON] Text

type TickerizeApi = 
  "tickerize" :> Capture "input" Text :> Get '[JSON] Text
  :<|> "slack" :> "tickerize" :> ReqBody '[FormUrlEncoded] SlackPayload :> Post '[JSON] SlackResponse

type RootEndpoint = Get '[HTML] (Html ())

type Api = HealthApi :<|> TickerizeApi :<|> RootEndpoint

data SlackPayload = SlackPayload {
  token :: Text,
  team_id :: Text,
  team_domain :: Text,
  enterprise_id :: Text,
  enterprise_name :: Text,
  channel_id :: Text,
  channel_name :: Text,
  user_id :: Text,
  user_name :: Text,
  command :: Text,
  text :: Text,
  response_url :: Text,
  trigger_id :: Text,
  api_app_id :: Text
} deriving stock (Eq, Show, Generic)

instance FromForm SlackPayload

data SlackResponse = SlackResponse {
  text :: Text,
  response_type :: Text
} deriving stock (Eq, Show, Generic)

instance FromJSON SlackResponse
instance ToJSON SlackResponse where
  toEncoding = genericToEncoding defaultOptions 

rootEndpoint :: Web sig m => ServerT RootEndpoint m
rootEndpoint = renderHome
  where
    renderHome :: Web sig m => m (Html ())
    renderHome = do
      AppState{baseUrl} <- ask @AppState
      return . body_ $ do
        p_ "To use this site, navigate to " <> with a_ [href_  (openBaseUrl baseUrl <> "/tickerize/allegedly%20brainstorming")] "Here"

tickerizeApi :: Web sig m => ServerT TickerizeApi m
tickerizeApi = tickerize :<|> slackTickerize
  where
    tickerize :: Web sig m => Text -> m Text
    tickerize input = do
          AppState{tickerTrie = t} <- ask @AppState
          return . T.unwords $ processInput t <$> T.words input
    
    slackTickerize :: Web sig m => SlackPayload -> m SlackResponse
    slackTickerize SlackPayload{text = input} = do
          AppState{tickerTrie = t} <- ask @AppState
          return SlackResponse {
            text = T.unwords $ processInput t <$> T.words input, 
            response_type = "ephemeral"
          }

    processInput :: Trie () -> Text -> Text
    processInput t input = 
      let firstSymbol = doLookup input
          (_, firstRemainder) = T.splitAt (T.length firstSymbol) input
          (secondSymbol, secondRemainder) = handleChunk firstRemainder
          (thirdSymbol, leftover) = handleChunk secondRemainder
      in firstSymbol <> secondSymbol <> thirdSymbol <> leftover
      where
        doLookup x = case Trie.match t . encodeUtf8 $ T.toUpper x of
            Nothing -> x
            Just (x0, _, _) -> decodeUtf8 x0
        
        handleChunk x = case T.length x of
            0 -> (x, "")
            1 -> (x, "")
            _ -> let 
              firstLetter = T.take 1 x
              remaining = T.drop 1 x
              looked = doLookup remaining
              (_, leftover) = T.splitAt (T.length looked) remaining
              in (firstLetter <> looked, leftover)

apiProxy :: Proxy Api
apiProxy = Proxy

server :: ServerT Api WebConcrete
server = healthApi :<|> tickerizeApi :<|> rootEndpoint
  where
    healthApi :: Web sig m => ServerT HealthApi m
    healthApi = return "I'm alive!"

appToHandler :: AppState -> WebConcrete a -> Handler a
appToHandler appEnv =
  Handler
    . ExceptT
    . try
    . runM
    . runReader @AppState appEnv

runServer :: AppState -> IO ()
runServer appState@AppState {port = port} = do
  print (("Running server on port " <> show port) :: Text)
  run (openPort port) app
  where
    handler :: WebConcrete a -> Handler a
    handler = appToHandler appState

    app :: Application
    app =
      serve apiProxy $
        hoistServer 
          apiProxy
          handler
          server