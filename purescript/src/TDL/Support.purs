module TDL.Support
  ( module Data.Argonaut.Core
  , module Data.Either
  , module Prelude
  , eqArray
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

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex = unsafePartial Array.unsafeIndex
