{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Configuration
    ( loadConfiguration,
        Configuration(..),
        QueryParameters
    ) where

import GHC.Generics
import Data.Yaml
import qualified Data.Map.Strict as Map

type QueryParameters = Map.Map String String
type Actions = Map.Map String QueryParameters
data Configuration = Configuration {
    device :: String,
    kodi :: String,
    actions :: Actions 
} deriving (Generic, Show)

instance FromJSON Configuration

loadConfiguration :: IO Configuration
loadConfiguration = decodeFileThrow "rfid-kodi.yaml"