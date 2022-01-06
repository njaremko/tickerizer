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
    openPort,
    openBaseUrl
  )
where

import Control.Carrier.Lift (Has, Lift, LiftC, runM, sendIO, sendM)
import Control.Carrier.Reader (Reader, ReaderC, ask, runReader)
import Control.Exception (throwIO, try)
import Relude hiding (Reader, ask, id, runReader)
import System.Envy (FromEnv, ToEnv)
import qualified System.Envy as Envy

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