module TDL.Support
  ( module Data.Argonaut.Core
  , module Data.ByteString
  , module Data.Either
  , module Data.Maybe
  , module Data.StrMap
  , module Prelude
  , eqArray
  , hash
  , fromProduct
  , fromSum
  , toProduct
  , toSum
  ) where

import Crypt.Hash.SHA256 (sha256)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Either (Either(..), either)
import Data.Foldable (and, foldMap)
import Data.Foldable (and, foldr)
import Data.Int (round)
import Data.Int as Int
import Data.Int.Bits ((.&.), shr)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.StrMap (StrMap, lookup)
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Prelude
import TDL.Intermediate (Intermediate(..), toText)
import TDL.Serializers.JSON (serialize)

eqArray :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
eqArray f a b = Array.length a == Array.length b && and (Array.zipWith f a b)

hash :: forall a. (a -> Intermediate) -> a -> ByteString
hash = \s x -> go (s x) # sha256
  where
    go Null        = tagNull
    go (String s)  = tagString <> i32be (BS.length u) <> u where u = BS.toUTF8 s
    go (Bytes b)   = tagBytes  <> i32be (BS.length b) <> b
    go (I32 i)     = tagI32    <> i32be i
    go (F64 f)     = tagF64    <> i32be (BS.length u) <> u where u = BS.toUTF8 (show f)
    go (Bool b)    = tagBool   <> BS.singleton (if b then 1 else 0)
    go (Array xs)  = tagArray  <> i32be (Array.length xs) <> foldMap go xs
    go (Object xs) = tagObject <> i32be (round $ StrMap.size xs) <> foldMap pr prs
      where pr (k /\ v) = go (String k) <> go v
            -- StrMap is bad and guarantees no order. In fact StrMap.toList is
            -- an impure function. Hence convert to a Map first, which does
            -- guarantee order (by always being sorted).
            prs = Map.toList $ Map.fromFoldable $ StrMap.toList xs

    tagNull   = BS.singleton 0
    tagString = BS.singleton 1
    tagBytes  = BS.singleton 2
    tagI32    = BS.singleton 3
    tagF64    = BS.singleton 4
    tagBool   = BS.singleton 5
    tagArray  = BS.singleton 6
    tagObject = BS.singleton 7

    -- TODO: https://github.com/rightfold/purescript-bytestrings/issues/5
    i32be i = BS.pack [ (i `shr` 24) .&. 0xFF
                      , (i `shr` 16) .&. 0xFF
                      , (i `shr`  8) .&. 0xFF
                      , (i `shr`  0) .&. 0xFF
                      ]

fromProduct :: Array {k :: String, v :: Intermediate} -> Intermediate
fromProduct = Object <<< foldr (\{k, v} -> StrMap.insert k v) StrMap.empty

fromSum :: forall a. String -> (a -> Intermediate) -> a -> Intermediate
fromSum k f x = Array [String k, f x]

toProduct :: Intermediate -> Either String (StrMap Intermediate)
toProduct (Object o) = Right o
toProduct _ = Left "Product was not serialized as an object."

toSum :: Intermediate -> Either String {d :: String, x :: Intermediate}
toSum (Array xs) = do
  case xs of
    [jd, x] -> {d: _, x} <$> toText jd
    _ -> Left "Sum was serialized as an array of the wrong length."
toSum _ =
  Left "Sum was not serialized as an array."
