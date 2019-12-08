{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Kodi
    ( playerOpen
    ) where
import GHC.Generics
import Data.Aeson

-- Simple structure of kodi jsonrpc message to open a given file
--'{"jsonrpc":"2.0","id":"1","method":"Player.Open","params":{"item":{"file":"url of file to play"}}}'

data JsonRPCFileItem = JsonRPCFileItem {
    file :: String
} deriving (Generic, Show)

data JsonRPCParams = JsonRPCParams {
    item :: JsonRPCFileItem
} deriving (Generic, Show)

data JsonRPC = JsonRPC{
    jsonrpc :: String,
    identity :: String,
    method :: String,
    params :: JsonRPCParams
} deriving (Generic, Show)


instance ToJSON JsonRPCFileItem
instance ToJSON JsonRPCParams

instance ToJSON JsonRPC where
    -- this generates a Value
    toJSON (JsonRPC jsonrpc identity method params) =
        object [
            "jsonrpc" .= jsonrpc,
            "id" .= identity,
            "method" .= method,
            "params" .= params
        ]

    -- this encodes directly to a bytestring Builder
    toEncoding (JsonRPC jsonrpc identity method params) =
        pairs (
            "jsonrpc" .= jsonrpc
            <> "id" .= identity
            <> "method" .= method
            <> "params" .= params
        )


playerOpen :: String -> JsonRPC
playerOpen url = JsonRPC {
    jsonrpc = "2.0",
    identity = "1",
    method = "Player.Open",
    params = JsonRPCParams {
        item = JsonRPCFileItem {
            file = url
        }
    }
}