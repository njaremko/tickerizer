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
import System.Envy (FromEnv, ToEnv)
import qualified System.Envy as Envy
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)
import Data.Trie (Trie )
import qualified Data.Trie as TE
import Data.Text.Encoding as TTE
import Servant.HTML.Lucid
import Lucid (Html, body_, p_, a_, With (with), href_)

newtype Port = Port Int
  deriving newtype (Envy.Var)

instance FromEnv Port where
  fromEnv _ = Port <$> Envy.env "PORT"

instance ToEnv Port where
  toEnv port = Envy.makeEnv [ "PORT" Envy..= port ]

newtype BaseUrl = BaseUrl {baseUrl :: Text}
  deriving newtype (Show, Semigroup, Monoid)

instance FromEnv BaseUrl where
  fromEnv _ = BaseUrl 
    <$> Envy.env "BASE_URL" 

instance ToEnv BaseUrl where
  toEnv BaseUrl {baseUrl} =
    Envy.makeEnv
      [ "BASE_URL" Envy..= baseUrl
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
    tickerTrie :: Trie ()
  }

appToHandler :: AppState -> WebConcrete a -> Handler a
appToHandler appEnv =
  Handler
    . ExceptT
    . try
    . runM
    . runReader @AppState appEnv

type HealthApi = "health" :> "alive" :> Get '[JSON] Text

type TickerizeApi = "tickerize" :> Capture "input" Text :> Get '[JSON] Text

type RootEndpoint = Get '[HTML] (Html ())

rootEndpoint :: Web sig m => ServerT RootEndpoint m
rootEndpoint = asdf
  where
    asdf :: Web sig m => m (Html ())
    asdf = do
      AppState{baseUrl = t} <- ask @AppState
      return $ body_ $ p_ "To use this site, navigate to " <> with a_ [href_  (coerce t <> "/tickerize/allegedly%20brainstorming")] "Here"

healthApi :: Web sig m => ServerT HealthApi m
healthApi = return "Ok"

tickerizeApi :: Web sig m => ServerT TickerizeApi m
tickerizeApi = tickerize 

tickerize :: Web sig m => Text -> m Text
tickerize input = do
      AppState{tickerTrie = t} <- ask @AppState
      return . T.unwords $ processInput t <$> T.words input

processInput :: Trie () -> Text -> Text
processInput t input = 
  let firstSymbol = doLookup input
      (_, firstRemainder) = T.splitAt (T.length firstSymbol) input
      (secondSymbol, secondRemainder) = handleChunk firstRemainder
      (thirdSymbol, leftover) = handleChunk secondRemainder
  in firstSymbol <> secondSymbol <> thirdSymbol <> leftover
  where
    doLookup x = case TE.match t . TTE.encodeUtf8 $ T.toUpper x of
        Nothing -> x
        Just (x0, _, _) -> TTE.decodeUtf8 x0
    
    handleChunk x = case T.length x of
        0 -> (x, "")
        1 -> (x, "")
        _ -> let 
          firstLetter = T.take 1 x
          remaining = T.drop 1 x
          looked = doLookup remaining
          (_, leftover) = T.splitAt (T.length looked) remaining
          in (firstLetter <> looked, leftover)

type Api = HealthApi :<|> TickerizeApi :<|> RootEndpoint

apiProxy :: Proxy Api
apiProxy = Proxy

server :: ServerT Api WebConcrete
server = healthApi :<|> tickerizeApi :<|> rootEndpoint

runServer :: AppState -> IO ()
runServer appState@AppState {port = Port port} = do
  print (("Running server on port " <> show port) :: Text)
  run port app
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