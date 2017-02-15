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
import Data.Array as Array
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Either (Either(..), either)
import Data.Foldable (and, foldMap)
import Data.Int (round)
import Data.Int.Bits ((.&.), shr)
import Data.Map as Map
import Data.StrMap as StrMap
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Prelude
import TDL.Intermediate (Intermediate(..))

eqArray :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
eqArray f a b = Array.length a == Array.length b && and (Array.zipWith f a b)

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex = unsafePartial Array.unsafeIndex

hash :: forall a. (a -> Intermediate) -> a -> ByteString
hash = \s x -> go (s x) # sha256
  where
    go Null = BS.singleton 0
    go (String s) = BS.singleton 1 <> i32be (BS.length u) <> u
      where u = BS.toUTF8 s
    go (Bytes b) = BS.singleton 2 <> i32be (BS.length b) <> b
    go (I32 i) = BS.singleton 3 <> i32be i
    go (F64 f) = BS.singleton 4 <> i32be (BS.length u) <> u
      where u = BS.toUTF8 (show f)
    go (Bool b) = BS.pack [5, if b then 1 else 0]
    go (Array xs) = BS.singleton 6 <> i32be (Array.length xs) <> foldMap go xs
    go (Object xs) = BS.singleton 7 <> i32be (round $ StrMap.size xs) <> foldMap pr prs
      where pr (k /\ v) = go (String k) <> go v
            -- StrMap is bad and guarantees no order. In fact StrMap.toList is
            -- an impure function. Hence convert to a Map first, which does
            -- guarantee order (by always being sorted).
            prs = Map.toList $ Map.fromFoldable $ StrMap.toList xs

    -- TODO: https://github.com/rightfold/purescript-bytestrings/issues/5
    i32be i = BS.pack [ (i `shr` 24) .&. 0xFF
                      , (i `shr` 16) .&. 0xFF
                      , (i `shr`  8) .&. 0xFF
                      , (i `shr`  0) .&. 0xFF
                      ]
