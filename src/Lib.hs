{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( openAndReadFrom
    ) where

-- import System.IO
import Evdev
import Evdev.Codes (EventType(EvKey),  Key(..))
import Control.Exception (finally)
-- import Data.Set (singleton)
import qualified Data.ByteString.Char8 as C

nextKey :: Device -> IO Key
nextKey device = do
    event <- nextEvent device
    case event of
        Event (KeyEvent key Pressed) _ -> return key
        _ -> nextKey device

represent :: Key -> String
represent Key1 = "1"
represent Key2 = "2"
represent Key3 = "3"
represent Key4 = "4"
represent Key5 = "5"
represent Key6 = "6"
represent Key7 = "7"
represent Key8 = "8"
represent Key9 = "9"
represent Key0 = "0"
represent KeyEnter = "\n" 
represent _ = "?"

collectLine :: Device -> String -> IO String
collectLine device buffer = do
    k <- nextKey device
    if k == KeyEnter
        then return buffer
        else collectLine device (buffer ++ represent k)

useDevice :: Device -> (String -> IO()) -> IO()
useDevice device codeHandler = do
    event <- nextEvent device
    line <- collectLine device ""
    codeHandler line
    useDevice device codeHandler

openAndReadFrom :: FilePath -> (String -> IO()) -> IO()
openAndReadFrom devicePath codeHandler = do
    device <- newDevice (C.pack devicePath)
    _ <- grabDevice device
    finally (useDevice device codeHandler) (ungrabDevice device)
    