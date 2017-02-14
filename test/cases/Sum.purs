module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Maybe(..), One(..), intermediateToMaybe, intermediateFromMaybe)

left :: Unit -> Boolean
left _ = intermediateToMaybe (intermediateFromMaybe u) == Right u
    where u = Maybe_just 42

right :: Unit -> Boolean
right _ = intermediateToMaybe (intermediateFromMaybe u) == Right u
    where u = Maybe_nothing (One {})
