{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Kodi
import Data.Text (Text (..))
import Data.Aeson

main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            (encode (playerOpen "url of file to play")) `shouldBe` "{\"jsonrpc\":\"2.0\",\"params\":{\"item\":{\"file\":\"url of file to play\"}},\"method\":\"Player.Open\",\"id\":\"1\"}"
