{-# LANGUAGE TupleSections #-}
module Main (main) where

import Tickerizer 
import Relude 
import System.Envy (decodeEnv)
import Data.Text (pack)
import qualified Configuration.Dotenv as Dotenv
import Constants (tickers)
import qualified Data.Trie as SF 

loadAppState :: IO AppState
loadAppState = do
  baseUrl <-
    decodeEnv >>= \case
      Left e -> error $ pack e
      Right v -> return v
  port <-
    decodeEnv >>= \case
      Left e -> error $ pack e
      Right v -> return v

  let tickerTrie = SF.fromList $ fmap (, ()) tickers

  return
    AppState
      { baseUrl,
        port,
        tickerTrie
      }

main :: IO ()
main = do
  Dotenv.onMissingFile (void $ Dotenv.loadFile Dotenv.defaultConfig) (return ())
  appState <- loadAppState
  runServer appState
