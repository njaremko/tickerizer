module Main (main) where

import Tickerizer (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
