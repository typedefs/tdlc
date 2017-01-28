module TDL.Extraction.PureScript
  ( pursTypeName
  , pursSerialize
  , pursModule
  , pursDeclaration
  ) where

import Data.Array as Array
import Data.Foldable (fold, foldMap, foldr)
import Data.List ((:), List(Nil))
import Data.List as List
import Data.String as String
import Data.Tuple.Nested ((/\))
import Prelude
import TDL.Syntax (Declaration(..), Kind(..), Module, PrimType(..), Type(..))

pursTypeName :: Type -> String
pursTypeName (NamedType n) = n
pursTypeName (PrimType I32Type)  = "Int"
pursTypeName (PrimType F64Type)  = "Number"
pursTypeName (PrimType TextType) = "String"
pursTypeName (ProductType ts) = "{" <> String.joinWith ", " entries <> "}"
  where entries = map (\(k /\ t) -> k <> " :: " <> pursTypeName t) ts
pursTypeName (SumType ts) = foldr step "TDLSUPPORT.Void" ts
  where step (_ /\ t) u = "(TDLSUPPORT.Either " <> pursTypeName t <> " " <> u <> ")"
pursTypeName (FuncType a b) = "(" <> pursTypeName a <> " -> " <> pursTypeName b <> ")"

-- | This function may throw on ill-typed inputs.
pursSerialize :: Partial => Type -> String
pursSerialize (NamedType n) = "serialize" <> n
pursSerialize (PrimType I32Type)  = "TDLSUPPORT.serializeI32"
pursSerialize (PrimType F64Type)  = "TDLSUPPORT.serializeF64"
pursSerialize (PrimType TextType) = "TDLSUPPORT.serializeText"
pursSerialize (ProductType ts) =
  "(\\tdl__r -> TDLSUPPORT.serializeProduct [" <> String.joinWith ", " entries <> "])"
  where entries = map (\(k /\ t) -> pursSerialize t <> " tdl__r." <> k) ts
pursSerialize (SumType ts) = go 0 (List.fromFoldable ts)
  where go _ Nil = "TDLSUPPORT.absurd"
        go n ((_ /\ head) : tail) =
          "(TDLSUPPORT.either "
          <> "(TDLSUPPORT.serializeVariant " <> show n <> " " <> pursSerialize head <> ") "
          <> go (n + 1) tail
          <> ")"

-- | This function may throw on ill-typed inputs.
pursDeserialize :: Partial => Type -> String
pursDeserialize (NamedType n) = "deserialize" <> n
pursDeserialize (PrimType I32Type)  = "TDLSUPPORT.deserializeI32"
pursDeserialize (PrimType F64Type)  = "TDLSUPPORT.deserializeF64"
pursDeserialize (PrimType TextType) = "TDLSUPPORT.deserializeText"
pursDeserialize (ProductType ts) =
  "(\\tdl__r -> "
  <> "TDLSUPPORT.deserializeProduct " <> show (Array.length ts) <> " tdl__r"
  <> " TDLSUPPORT.>>= \\tdl__r' ->\n"
  <> record <> ")"
  where
    record
      | Array.length ts == 0 = "  TDLSUPPORT.pure {}"
      | otherwise = indent (
            "{" <> String.joinWith ", " (map (\(k /\ _) -> k <> ": _") ts) <> "}\n"
            <> "TDLSUPPORT.<$> "
            <> String.joinWith "\nTDLSUPPORT.<*> " (Array.mapWithIndex entry ts)
          )
    entry i (_ /\ t) =
      pursDeserialize t <> " (TDLSUPPORT.unsafeIndex tdl__r' " <> show i <> ")"
pursDeserialize (SumType ts) =
  "(\\tdl__r ->"
  <> " TDLSUPPORT.deserializeSum tdl__r"
  <> " TDLSUPPORT.>>= case _ of\n" <> fold (Array.mapWithIndex entry ts)
  <> "  {d: _} -> TDLSUPPORT.Left " <> show "Sum discriminator was out of bounds."
  <> ")"
  where entry i (_ /\ t) =
          "  {d: " <> show i <> ", x: tdl__x} -> " <> path i <> " TDLSUPPORT.<$>\n"
          <> indent (indent (pursDeserialize t)) <> " tdl__x\n"
        path n | n <= 0    = "TDLSUPPORT.Left"
               | otherwise = "TDLSUPPORT.Right TDLSUPPORT.<<< " <> path (n - 1)

-- | This function may throw on ill-typed inputs.
pursModule :: Partial => Module -> String
pursModule m =
     "import TDL.Support as TDLSUPPORT\n"
  <> foldMap pursDeclaration m

-- | This function may throw on ill-typed inputs.
pursDeclaration :: Partial => Declaration -> String
pursDeclaration (TypeDeclaration n k t) =
     "newtype " <> n <> " = " <> n <> " " <> pursTypeName t <> "\n"
  <> serialization
  where serialization = case k of
          TypeKind -> ""
          SeriKind ->
              "derive instance eq" <> n <> " :: TDLSUPPORT.Eq " <> n <> "\n"
            <> "serialize" <> n <> " :: " <> n <> " -> TDLSUPPORT.Json\n"
            <> "serialize" <> n <> " (" <> n <> " tdl__a) =\n"
            <> "  " <> pursSerialize t <> " tdl__a\n"
            <> "deserialize" <> n
            <> " :: TDLSUPPORT.Json -> TDLSUPPORT.Either String " <> n <> "\n"
            <> "deserialize" <> n <> " =\n"
            <> indent (pursDeserialize t) <> "\n"
            <> "  TDLSUPPORT.>>> TDLSUPPORT.map " <> n <> "\n"

indent :: String -> String
indent =
  String.split (String.Pattern "\n")
  >>> map ("  " <> _)
  >>> String.joinWith "\n"
