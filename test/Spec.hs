{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Text (Text (..))
import Kodi
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Json RPC kodi instances" $ do
    it "encode playerOpen correctly" $ do
      (encode (playerOpen "url of file to play")) `shouldBe` "{\"jsonrpc\":\"2.0\",\"params\":{\"item\":{\"file\":\"url of file to play\"}},\"method\":\"Player.Open\",\"id\":\"1\"}"
