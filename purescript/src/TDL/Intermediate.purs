module TDL.Intermediate
where

import Data.Array as Array
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Either (Either(..), either)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Global (nan, infinity)
import Node.Encoding (Encoding (..)) as BS
import Prelude

data Intermediate
  = Null
  | String String
  | Bytes ByteString
  | I32 Int
  | F64 Number
  | Bool Boolean
  | Array (Array Intermediate)
  | Object (StrMap Intermediate)

fromI32 :: Int -> Intermediate
fromI32 = I32

fromF64 :: Number -> Intermediate
fromF64 = F64

fromBool :: Boolean -> Intermediate
fromBool = Bool

fromText :: String -> Intermediate
fromText = String

fromBytes :: ByteString -> Intermediate
fromBytes = Bytes

fromArray :: forall a. (a -> Intermediate) -> Array a -> Intermediate
fromArray f = Array <<< map f

toI32 :: Intermediate -> Either String Int
toI32 (I32 i) =
  pure i
toI32 (F64 f) =
  maybe (Left "i32 was not serialized as an integral number.") Right $
    Int.fromNumber f
toI32 _ =
  Left "i32 was not serialized as an integral number."

toF64 :: Intermediate -> Either String Number
toF64 (F64 f) = Right f
toF64 (I32 i) = Right (Int.toNumber i)
toF64 (String "NaN") = Right nan
toF64 (String "+∞") = Right infinity
toF64 (String "-∞") = Right (negate infinity)
toF64 _ = Left "f64 was not serialized as a number or NaN/∞ indicator."

toBool :: Intermediate -> Either String Boolean
toBool (Bool b) = Right b
toBool _ = Left "bool was not serialized as a Boolean."

toText :: Intermediate -> Either String String
toText (String str) = Right str
toText _ = Left "text was not serialized as a string."

toBytes :: Intermediate -> Either String ByteString
toBytes (Bytes bytes) = Right bytes
toBytes (String str) = do
  let decoded = BS.fromString str BS.Base64
  if BS.toString decoded BS.Base64 == str
    then Right decoded
    else Left "bytes was not serialized as a base64-encoded string"
toBytes _ = Left "bytes was not serialized as a string or byte array"

toArray :: forall a. (Intermediate -> Either String a) -> Intermediate -> Either String (Array a)
toArray f (Array xs) = traverse f xs
toArray _ _ = Left "array was not serialized as an array."
