module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Pax(..), deserializePax, serializePax)

pax :: Unit -> Boolean
pax _ = deserializePax (serializePax (Pax 1)) == Right (Pax 1)
