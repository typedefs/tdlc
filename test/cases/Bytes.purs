module Main where

import Data.ByteString as ByteString
import Data.Either (Either(..))
import Prelude
import TDLOutput (Bytes(..), intermediateToBytes, intermediateFromBytes)

bytes :: Unit -> Boolean
bytes _ = intermediateToBytes (intermediateFromBytes u) == Right u
    where u = Bytes $ ByteString.pack [1, 2, 3, 4, 5]
