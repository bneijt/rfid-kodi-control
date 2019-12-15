{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Configuration
    ( loadConfiguration,
        Configuration(..)
    ) where

import GHC.Generics
import Data.Yaml
import qualified Data.Map.Strict as Map

type Actions = Map.Map String [String]

data Configuration = Configuration {
    device :: String,
    kodi :: String,
    actions :: Actions 
} deriving (Generic, Show)

instance FromJSON Configuration

loadConfiguration :: IO Configuration
loadConfiguration = decodeFileThrow "rfid-kodi.yaml"