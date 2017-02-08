module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (OK(..), intermediateToOK, intermediateFromOK)

ok :: Unit -> Boolean
ok _ = intermediateToOK (intermediateFromOK (OK true)) == Right (OK true)
