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
  toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue}

instance ToJSON JsonRPCParams

fieldRenames :: String -> String
fieldRenames "identity" = "id"
fieldRenames name = name

instance ToJSON JsonRPC where
  toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue, fieldLabelModifier = fieldRenames}

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
