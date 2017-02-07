module TDL.Serializers.JSON
  ( module Data.Argonaut.Core
  , module TDL.Intermediate
  , serialize
  , deserialize
  ) where

import TDL.Intermediate (Intermediate (..))
import TDL.Intermediate as Intermediate
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Data.ByteString (ByteString)
import Data.ByteString (toString) as BS
import Node.Encoding (Encoding (..)) as BS
import Control.Alt ( (<|>) )
import Prelude

serialize :: Intermediate -> Json
serialize Intermediate.Null = Json.jsonNull
serialize (I32 i) = Json.fromNumber <<< Int.toNumber $ i
serialize (F64 f) = Json.fromNumber f
serialize (Boolean b) = Json.fromBoolean b
serialize (String t) = Json.fromString t
serialize (Blob b) = Json.fromString $ BS.toString b BS.Base64
serialize (Array xs) = Json.fromArray <<< map serialize $ xs
serialize (Object xs) = Json.fromObject <<< map serialize $ xs

deserialize :: Json -> Intermediate
deserialize json = fromMaybe Intermediate.Null $
  (Object <<< map deserialize <$> Json.toObject json) <|>
  (Array <<< map deserialize <$> Json.toArray json) <|>
  (F64 <$> Json.toNumber json) <|>
  (Boolean <$> Json.toBoolean json) <|>
  (String <$> Json.toString json)

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex = unsafePartial Array.unsafeIndex
