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
import Network.Wai.Handler.Warp (runSettings, setLogger, setPort, defaultSettings)
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Servant.HTML.Lucid ( HTML )
import Lucid (Html, body_, p_, a_, With (with), href_)
import Data.Aeson (Value)
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req ((=:))
import Network.Wai.Logger 

type HealthApi = "health" :> "alive" :> Get '[JSON] Text

type TickerizeApi = 
  "tickerize" :> Capture "input" Text :> Get '[JSON] Text
  :<|> "slack" :> "tickerize" :> ReqBody '[FormUrlEncoded] SlackPayload :> Post '[JSON] SlackResponse

type RootEndpoint = Get '[HTML] (Html ())

type SlackApi = 
  "slack" :> Get '[JSON] Text :<|>
  "slack" :> "oauth" :> QueryParam "code" Text :> Get '[JSON] Value

type Api = HealthApi :<|> TickerizeApi :<|> SlackApi :<|> RootEndpoint

slackApi :: Web sig m => ServerT SlackApi m
slackApi = doRedirect :<|> doOauth
  where
    doRedirect = do
      AppState{slackClientId} <- ask
      sendIO . throwIO $ err301 { errHeaders = [("Location", "https://slack.com/oauth/v2/authorize?scope=commands&client_id=" <> encodeUtf8 (openSlackClientId slackClientId))] }
    doOauth :: Web sig m => Maybe Text -> m Value
    doOauth code = do
      AppState{slackClientId, slackClientSecret} <- ask
      sendIO $ Req.runReq Req.defaultHttpConfig $ do
                  let params =
                          "client_id" =: openSlackClientId slackClientId <>
                          "client_secret" =: openSlackClientSecret slackClientSecret <>
                          "code" =: fromMaybe "" code <>
                          "grant_type" =: ("authorization_code" :: Text)
                  v <- Req.req Req.POST (Req.https "slack.com" Req./: "api" Req./: "oauth.v2.access") (Req.ReqBodyUrlEnc params) Req.jsonResponse mempty
                  return (Req.responseBody v :: Value)

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
          text <- tickerize input
          return SlackResponse {
            text, 
            response_type = "in_channel"
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
server = healthApi :<|> tickerizeApi :<|> slackApi :<|> rootEndpoint
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
  withStdoutLogger $ \aplogger -> do
        let settings = setPort (openPort port) $ setLogger aplogger defaultSettings
        runSettings settings app
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