module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Maybe(..), One(..), deserializeMaybe, serializeMaybe)

left :: Unit -> Boolean
left _ = deserializeMaybe (serializeMaybe u) == Right u
    where u = Maybe (Left 42)

right :: Unit -> Boolean
right _ = deserializeMaybe (serializeMaybe u) == Right u
    where u = Maybe (Right (Left (One {})))
