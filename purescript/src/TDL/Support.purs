module TDL.Support
  ( module Data.Argonaut.Core
  , module Data.Either
  , module Prelude
  , serializeInt
  , serializeProduct
  , serializeVariant
  , deserializeInt
  , deserializeProduct
  , deserializeSum
  , unsafeIndex
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Int as Int
import Data.Maybe (maybe)
import Partial.Unsafe (unsafePartial)
import Prelude

serializeInt :: Int -> Json
serializeInt = Json.fromNumber <<< Int.toNumber

serializeProduct :: Array Json -> Json
serializeProduct = Json.fromArray

serializeVariant :: forall a. Int -> (a -> Json) -> a -> Json
serializeVariant n f x = Json.fromArray [Json.fromNumber (Int.toNumber n), f x]

deserializeInt :: Json -> Either String Int
deserializeInt =
  (Json.toNumber >=> Int.fromNumber)
  >>> maybe (Left "Integer was not serialized as an integral JSON number.") Right

deserializeProduct :: Int -> Json -> Either String (Array Json)
deserializeProduct n j = do
  a <- Json.toArray j # maybe (Left "Product was not serialized as a JSON array.") Right
  when (Array.length a /= n) $
    Left "Product was serialized as a JSON array of the wrong length."
  pure a

deserializeSum :: Int -> Json -> Either String {d :: Int, x :: Json}
deserializeSum n j = do
  a <- Json.toArray j # maybe (Left "Sum was not serialized as a JSON array.") Right
  case a of
    [jd, x] -> {d: _, x} <$> deserializeInt jd
    _ -> Left "Sum was serialized as a JSON array of the wrong length."

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex = unsafePartial Array.unsafeIndex
