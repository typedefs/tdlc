module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (Ints(..), deserializeInts, serializeInts)

ints :: Unit -> Boolean
ints _ = deserializeInts (serializeInts u) == Right u
    where u = Ints [1, 2, 3, 4, 5]
