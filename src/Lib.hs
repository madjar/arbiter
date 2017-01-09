{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app
    ) where

import qualified Network.Wai as WAI
import Network.HTTP.Types
import qualified Network.HTTP.Client as HC
import           Control.Exception          (try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent.Async
import Data.List.Extra
import Control.Arrow ((&&&))
import Data.Ord
import Data.Either
import System.Console.Regions
import System.Console.Concurrent
import Control.Concurrent

data CandidateResponse =  CandidateResponse Status LBS.ByteString deriving Show

backs = [("127.0.0.1", 5555), ("127.0.0.1", 5556)]

app :: WAI.Application
app req respond = displayConsoleRegions $ withConsoleRegion Linear $ \r -> do
    responses <- mapConcurrently (queryBackend r req) backs
    let (status, body) = buildResponse responses

    finishConsoleRegion r (show status)

    respond $ WAI.responseLBS
        status
        []
        body


queryBackend :: ConsoleRegion -> WAI.Request -> (BS.ByteString, Int) -> IO (Either HC.HttpException (HC.Response LBS.ByteString))
queryBackend region req (host, port) = withConsoleRegion (InLine region) $ \r -> do
    --print ("yay" :: String)
    let display = show host ++ ":" ++ show port ++ " "
    setConsoleRegion r display
    threadDelay 1000000
    eresponse <- forwardRequest req (host, port)
    setConsoleRegion r (display ++ " -> " ++ take 15 (show eresponse) ++ " ")
    threadDelay 1000000
    return eresponse -- TODO switch to the other type

forwardRequest :: WAI.Request -> (BS.ByteString, Int) -> IO (Either HC.HttpException (HC.Response LBS.ByteString))
forwardRequest req (host, port)= do
    body <- WAI.strictRequestBody req
    manager <- HC.newManager HC.defaultManagerSettings
    let request = HC.defaultRequest
                    { HC.checkStatus = \_ _ _ -> Nothing
                    , HC.responseTimeout = Nothing
                    , HC.method = WAI.requestMethod req
                    , HC.secure = False
                    , HC.host = host
                    , HC.port = port
                    , HC.path = WAI.rawPathInfo req
                    , HC.queryString = WAI.rawQueryString req
                    , HC.requestHeaders = WAI.requestHeaders req
                    , HC.requestBody = HC.RequestBodyLBS body
                    , HC.redirectCount = 0
                    }

    try $ HC.httpLbs request manager


buildResponse :: [Either HC.HttpException (HC.Response LBS.ByteString)] -> (Status, LBS.ByteString)
buildResponse responses = (bestStatus, bestBody)
  where good = filter (statusIsSuccessful . HC.responseStatus) . rights $ responses
        statusesAndBodies = map (HC.responseStatus &&& HC.responseBody) good
        (bestStatus, bestBodies) = maximumBy (comparing $ length . snd) . groupSort $ statusesAndBodies
        bestBody = head . maximumBy (comparing length) $ group . sort $ bestBodies -- XX head

