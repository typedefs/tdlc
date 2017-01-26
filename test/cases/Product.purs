module Main where

import Data.Either (Either(..))
import Prelude
import TDLOutput (User(..), UserID(..), deserializeUser, serializeUser)

user :: Unit -> Boolean
user _ = deserializeUser (serializeUser user) == Right user
    where user = User { id: UserID 1, age: 2 }
