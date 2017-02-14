module TDL.Support
  ( module Data.Argonaut.Core
  , module Data.ByteString
  , module Data.Either
  , module Prelude
  , eqArray
  , unsafeIndex
  , hash
  ) where

import Crypt.Hash.SHA256 (sha256)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.Either (Either(..), either)
import Data.Foldable (and)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Prelude
import TDL.Intermediate (Intermediate)
import TDL.Serializers.JSON (serialize)

eqArray :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
eqArray f a b = Array.length a == Array.length b && and (Array.zipWith f a b)

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex = unsafePartial Array.unsafeIndex

hash :: forall a. (a -> Intermediate) -> a -> ByteString
hash s x = sha256 $ ByteString.toUTF8 $ Json.stringify $ serialize $ s x
