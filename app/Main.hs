{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where
import Lib
import Configuration

import GHC.Generics
-- import Data.Yaml
import qualified Data.Map.Strict as Map


import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import qualified Data.ByteString as B
import Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

callKodi :: Text -> QueryParameters -> IO ()
callKodi kodiHost params = runReq defaultHttpConfig $ do
  let requestBody = fromJust $ Map.lookup "request" params
  r <- req POST
    (http kodiHost /: "jsonrpc")
    (ReqBodyBs (C.pack requestBody)) 
    bsResponse 
    mempty 
  liftIO $ print (responseBody r :: B.ByteString)


action :: Configuration -> String -> IO ()
action config action = do
    let request = Map.lookup action (actions config)
    case request of
        Nothing -> putStrLn ("Action not found: " ++ action)
        Just parameters -> callKodi (T.pack $ kodi config) parameters

main = do
    config <- loadConfiguration
    openAndReadFrom (device config) (action config)

