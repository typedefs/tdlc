module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Pax(..), intermediateToPax, intermediateFromPax)

pax :: Unit -> Boolean
pax _ = intermediateToPax (intermediateFromPax (Pax 1)) == Right (Pax 1)
