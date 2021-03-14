{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kodi
  ( playerOpen,
    playerOpenDirectory,
  )
where

import Data.Aeson
import Data.Text (Text (..))
import qualified Data.Text as T
import GHC.Generics

-- https://kodi.wiki/view/JSON-RPC_API/v8
-- Simple structure of kodi jsonrpc message to open a given file

--
data JsonRPCItem
  = JsonRPCDirectoryItem
      { directory :: Text,
        recursive :: Bool
      }
  | JsonRPCFileItem
      { file :: Text
      }
  deriving (Generic, Show)

--
data JsonRPCParams = JsonRPCParams
  { item :: JsonRPCItem
  }
  deriving (Generic, Show)

data JsonRPC = JsonRPC
  { jsonrpc :: Text,
    identity :: Text,
    method :: Text,
    params :: JsonRPCParams
  }
  deriving (Generic, Show)

instance ToJSON JsonRPCItem where
  toJSON = genericToJSON defaultOptions {  sumEncoding  = UntaggedValue}

instance ToJSON JsonRPCParams

fieldRenames :: String -> String
fieldRenames "identity" = "id"
fieldRenames name = name


instance ToJSON JsonRPC where
  -- this generates a Value
  toJSON = genericToJSON defaultOptions {  sumEncoding  = UntaggedValue, fieldLabelModifier = fieldRenames }
  -- toJSON (JsonRPC jsonrpc identity method params) = 
  --   object
  --     [ "jsonrpc" .= jsonrpc,
  --       "id" .= identity,
  --       "method" .= method,
  --       "params" .= params
  --     ]

  -- -- this encodes directly to a bytestring Builder
  -- toEncoding (JsonRPC jsonrpc identity method params) =
  --   pairs
  --     ( "jsonrpc" .= jsonrpc
  --         <> "id" .= identity
  --         <> "method" .= method
  --         <> "params" .= params
  --     )

-- Mar 13 18:25:22 mediamonster rfid-kodi-control-exe[599]: "{\"error\":{\"code\":-32602,\"data\":{\"method\":\"Player.Open\",\"stack\":{\"message\":\"Received value does not match any of the union type definitions\",\"name\":\"item\",\"type\":\"object\"}},\"message\":\"Invalid params.\"},\"id\":\"1\",\"jsonrpc\":\"2.0\"}"



--'{"jsonrpc":"2.0","id":"1","method":"Player.Open","params":{"item":{"file":"url of file to play"}}}'
playerOpen :: Text -> JsonRPC
playerOpen url =
  JsonRPC
    { jsonrpc = "2.0",
      identity = "1",
      method = "Player.Open",
      params =
        JsonRPCParams
          { item =
              JsonRPCFileItem
                { file = url
                }
          }
    }

-- [{"jsonrpc":"2.0","method":"Player.PlayPause","params":[0,"toggle"],"id":43}]
playerOpenDirectory :: Text -> JsonRPC
playerOpenDirectory dir =
  JsonRPC
    { jsonrpc = "2.0",
      identity = "1",
      method = "Player.Open",
      params =
        JsonRPCParams
          { item =
              JsonRPCDirectoryItem
                { directory = dir,
                  recursive = True
                }
          }
    }
