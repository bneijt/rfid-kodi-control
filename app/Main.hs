{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration
-- import Data.Yaml

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import GHC.Generics
import Kodi
import Lib
import Network.HTTP.Req
import System.IO
import System.Random (randomRIO)
import Text.URI (URI)
import qualified Text.URI as URI

callKodi :: T.Text -> T.Text -> IO ()
callKodi kodiHost playUrl = do
  uri <- URI.mkURI kodiHost
  let reqBody = if T.isSuffixOf "/" playUrl then playerOpenDirectory playUrl else playerOpen playUrl
  let location = fromJust $ useHttpURI uri

  -- print location
  runReq defaultHttpConfig $ do
    r <-
      req
        POST
        (fst location)
        (ReqBodyJson reqBody)
        bsResponse
        (snd location)
    liftIO $ print (responseBody r :: B.ByteString)

randomPick :: [a] -> IO a
randomPick l = (l !!) <$> randomRIO (0, length l - 1)

action :: Configuration -> String -> IO ()
action config actionName =
  case Map.lookup actionName (actions config) of
    Nothing -> do
      putStrLn ("Action not found in configuration: " ++ actionName)
      hFlush stdout
    Just playUrls -> do
      playUrl <- randomPick playUrls
      putStrLn (actionName ++ " -> " ++ playUrl)
      callKodi (T.pack $ kodi config) (T.pack playUrl)
      hFlush stdout


main = do
  config <- loadConfiguration
  putStrLn $ "Loaded " ++ show (length (actions config)) ++ " actions from configuration"
  openAndReadFrom (device config) (action config)
