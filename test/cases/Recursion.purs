module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Cons(..), List(..), Nil(..), intermediateToList, intermediateFromList)

list :: Unit -> Boolean
list _ = intermediateToList (intermediateFromList u) == Right u
    where u = List $ Right $ Left (Cons {head: 1, tail: List $ Left (Nil {})})
