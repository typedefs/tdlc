module TDL.Support
  ( module Data.Argonaut.Core
  , module Data.Either
  , module Prelude
  , serializeInt
  , serializeProduct
  , deserializeInt
  , deserializeProduct
  , unsafeIndex
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (maybe)
import Partial.Unsafe (unsafePartial)
import Prelude

serializeInt :: Int -> Json
serializeInt = Json.fromNumber <<< Int.toNumber

serializeProduct :: Array Json -> Json
serializeProduct = Json.fromArray

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

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex = unsafePartial Array.unsafeIndex
