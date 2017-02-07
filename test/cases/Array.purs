module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Ints(..), intermediateToInts, intermediateFromInts)

ints :: Unit -> Boolean
ints _ = intermediateToInts (intermediateFromInts u) == Right u
    where u = Ints [1, 2, 3, 4, 5]
