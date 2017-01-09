module Main where

import Network.Wai.Handler.Warp (run)

import Lib

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
