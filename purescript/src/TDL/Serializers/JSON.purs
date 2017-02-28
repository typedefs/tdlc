module TDL.Serializers.JSON
  ( module Data.Argonaut.Core
  , module TDL.Intermediate
  , serialize
  , deserialize
  ) where

import Control.Alt ( (<|>) )
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.ByteString (toString) as BS
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Global (infinity, isNaN)
import Node.Encoding (Encoding (..)) as BS
import Prelude
import TDL.Intermediate (Intermediate (..))
import TDL.Intermediate as Intermediate

serialize :: Intermediate -> Json
serialize Intermediate.Null = Json.jsonNull
serialize (I32 i) = Json.fromNumber <<< Int.toNumber $ i
serialize (F64 f)
  -- https://github.com/purescript-contrib/purescript-argonaut/issues/34
  | isNaN f = Json.fromString "NaN"
  | f == infinity = Json.fromString "Inf"
  | f == (negate infinity) = Json.fromString "-Inf"
  | otherwise = Json.fromNumber f
serialize (Bool b) = Json.fromBoolean b
serialize (String t) = Json.fromString t
serialize (Bytes b) = Json.fromString $ BS.toString b BS.Base64
serialize (Array xs) = Json.fromArray <<< map serialize $ xs
serialize (Object xs) = Json.fromObject <<< map serialize $ xs

deserialize :: Json -> Intermediate
deserialize json = fromMaybe Intermediate.Null $
  (Object <<< map deserialize <$> Json.toObject json) <|>
  (Array <<< map deserialize <$> Json.toArray json) <|>
  (F64 <$> Json.toNumber json) <|>
  (Bool <$> Json.toBoolean json) <|>
  (String <$> Json.toString json)
