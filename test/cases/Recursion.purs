module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Cons(..), List(..), Nil(..), deserializeList, serializeList)

list :: Unit -> Boolean
list _ = deserializeList (serializeList u) == Right u
    where u = List $ Right $ Left (Cons {head: 1, tail: List $ Left (Nil {})})
