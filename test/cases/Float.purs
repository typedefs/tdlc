module Main where

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Global (infinity, isNaN, nan)
import Prelude
import TDL.Serializers.JSON (serialize, deserialize)
import TDLOutput (Load(..), intermediateToLoad, intermediateFromLoad)

_to = stringify <<< serialize <<< intermediateFromLoad
_from = (intermediateToLoad <<< deserialize) <=< jsonParser

float :: Unit -> Boolean
float _ = _from (_to (Load 3.14)) == Right (Load 3.14)

nan' :: Unit -> Boolean
nan' _ = case _from (_to (Load nan)) of
  Right (Load l) -> isNaN l
  _ -> false

posinf :: Unit -> Boolean
posinf _ = _from (_to (Load infinity)) == Right (Load infinity)

neginf :: Unit -> Boolean
neginf _ = _from (_to (Load (negate infinity))) == Right (Load (negate infinity))
