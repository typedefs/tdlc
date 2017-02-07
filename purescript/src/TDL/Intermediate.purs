module TDL.Intermediate
where

import Prelude
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Node.Encoding (Encoding (..)) as BS
import Data.Either (Either(..), either)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.StrMap

data Intermediate
  = Null
  | String String
  | Bytes ByteString
  | I32 Int
  | F64 Number
  | Boolean Boolean
  | Array (Array Intermediate)
  | Object (StrMap Intermediate)

fromI32 :: Int -> Intermediate
fromI32 = I32

fromF64 :: Number -> Intermediate
fromF64 = F64

fromText :: String -> Intermediate
fromText = String

fromBytes :: ByteString -> Intermediate
fromBytes = Bytes

fromArray :: forall a. (a -> Intermediate) -> Array a -> Intermediate
fromArray f = Array <<< map f

fromProduct :: Array Intermediate -> Intermediate
fromProduct = Array

fromVariant :: forall a. Int -> (a -> Intermediate) -> a -> Intermediate
fromVariant n f x = Array [I32 n, f x]

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
toF64 _ = Left "f64 was not serialized as a number."

toText :: Intermediate -> Either String String
toText (String str) = Right str
toText _ = Left "text was not serialized as a string."

toBytes :: Intermediate -> Either String ByteString
toBytes (Bytes bytes) = Right bytes
toBytes (String str) = do
  let decoded = BS.fromString str BS.Base64
  if BS.toString decoded BS.Base64 == str
    then Right decoded
    else Left "bytestring was not serialized as a base64-encoded string"
toBytes _ = Left "bytestring was not serialized as a string or byte array"

toArray :: forall a. (Intermediate -> Either String a) -> Intermediate -> Either String (Array a)
toArray f (Array xs) = traverse f xs
toArray _ _ = Left "array was not serialized as an array."

toProduct :: Int -> Intermediate -> Either String (Array Intermediate)
toProduct n (Array xs) =
  if Array.length xs == n
    then Right xs
    else Left "Product was serialized as an array of the wrong length."
toProduct _ _ =
  Left "Product was not serialized as an array."

toSum :: Intermediate -> Either String {d :: Int, x :: Intermediate}
toSum (Array xs) = do
  case xs of
    [jd, x] -> {d: _, x} <$> toI32 jd
    _ -> Left "Sum was serialized as an array of the wrong length."
toSum _ =
  Left "Sum was not serialized as an array."
