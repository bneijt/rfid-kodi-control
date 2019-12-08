{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where
import Lib
import Configuration

import GHC.Generics
-- import Data.Yaml
import qualified Data.Map.Strict as Map
import System.IO
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Kodi
import Text.URI (URI)
import qualified Text.URI as URI

callKodi :: T.Text -> String -> IO ()
callKodi kodiHost playUrl = do
  uri <- URI.mkURI kodiHost
  let location = fromJust $ useHttpURI uri
  -- print location
  runReq defaultHttpConfig $ do
    r <- req POST
      (fst location)
      (ReqBodyJson (playerOpen playUrl)) 
      bsResponse 
      (snd location) 
    liftIO $ print (responseBody r :: B.ByteString)


action :: Configuration -> String -> IO ()
action config actionName = do
    case Map.lookup actionName (actions config) of
        Nothing -> do
          putStrLn ("Action not found in configuration: " ++ actionName)
          hFlush stdout
        Just playUrl -> callKodi (T.pack $ kodi config) playUrl

main = do
    config <- loadConfiguration
    putStrLn $ "Loaded " ++ show (length (actions config)) ++ " actions from configuration"
    openAndReadFrom (device config) (action config)

