module TDL.Support
  ( module Data.Argonaut.Core
  , module Data.Either
  , module Prelude
  , eqArray
  , serializeI32
  , serializeF64
  , serializeText
  , serializeArray
  , serializeProduct
  , serializeVariant
  , deserializeI32
  , deserializeF64
  , deserializeText
  , deserializeArray
  , deserializeProduct
  , deserializeSum
  , unsafeIndex
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (and)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Prelude

eqArray :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
eqArray f a b = Array.length a == Array.length b && and (Array.zipWith f a b)

serializeI32 :: Int -> Json
serializeI32 = Json.fromNumber <<< Int.toNumber

serializeF64 :: Number -> Json
serializeF64 = Json.fromNumber

serializeText :: String -> Json
serializeText = Json.fromString

serializeArray :: forall a. (a -> Json) -> Array a -> Json
serializeArray f = Json.fromArray <<< map f

serializeProduct :: Array Json -> Json
serializeProduct = Json.fromArray

serializeVariant :: forall a. Int -> (a -> Json) -> a -> Json
serializeVariant n f x = Json.fromArray [Json.fromNumber (Int.toNumber n), f x]

deserializeI32 :: Json -> Either String Int
deserializeI32 =
  (Json.toNumber >=> Int.fromNumber)
  >>> maybe (Left "i32 was not serialized as an integral JSON number.") Right

deserializeF64 :: Json -> Either String Number
deserializeF64 =
  Json.toNumber
  >>> maybe (Left "f64 was not serialized as a JSON number.") Right

deserializeText :: Json -> Either String String
deserializeText =
  Json.toString
  >>> maybe (Left "text was not serialized as a JSON string.") Right

deserializeArray :: forall a. (Json -> Either String a) -> Json -> Either String (Array a)
deserializeArray f j = do
  a <- Json.toArray j # maybe (Left "array was not serialized as a JSON array.") Right
  traverse f a

deserializeProduct :: Int -> Json -> Either String (Array Json)
deserializeProduct n j = do
  a <- Json.toArray j # maybe (Left "Product was not serialized as a JSON array.") Right
  when (Array.length a /= n) $
    Left "Product was serialized as a JSON array of the wrong length."
  pure a

deserializeSum :: Json -> Either String {d :: Int, x :: Json}
deserializeSum j = do
  a <- Json.toArray j # maybe (Left "Sum was not serialized as a JSON array.") Right
  case a of
    [jd, x] -> {d: _, x} <$> deserializeI32 jd
    _ -> Left "Sum was serialized as a JSON array of the wrong length."

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex = unsafePartial Array.unsafeIndex
