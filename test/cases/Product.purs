module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (User(..), UserID(..), intermediateToUser, intermediateFromUser)

user :: Unit -> Boolean
user _ = intermediateToUser (intermediateFromUser u) == Right u
    where u = User { id: UserID "b23a446f-3797-4771-be82-6625b46c546f", age: 2 }
