module TDL.Extraction.PureScript
  ( pursTypeName
  , pursEq
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
import TDL.LambdaCalculus (etaExpandType)
import TDL.Syntax (Declaration(..), Kind(..), Module(..), PrimType(..), Type(..))

pursKindName :: Kind -> String
pursKindName SeriKind = "Type"
pursKindName (ArrowKind k i) = "(" <> pursKindName k <> " " <> pursKindName i <> ")"

pursTypeName :: Type -> String
pursTypeName (NamedType n) = n
pursTypeName (AppliedType t u) = "(" <> pursTypeName t <> " " <> pursTypeName u <> ")"
pursTypeName (PrimType BoolType)  = "Boolean"
pursTypeName (PrimType I32Type)   = "Int"
pursTypeName (PrimType F64Type)   = "Number"
pursTypeName (PrimType TextType)  = "String"
pursTypeName (PrimType ArrayType) = "Array"
pursTypeName (PrimType BytesType) = "TDLSUPPORT.ByteString"
pursTypeName (ProductType ts) = "{" <> String.joinWith ", " entries <> "}"
  where entries = map (\(k /\ t) -> k <> " :: " <> pursTypeName t) ts
pursTypeName (SumType ts) = foldr step "TDLSUPPORT.Void" ts
  where step (_ /\ t) u = "(TDLSUPPORT.Either " <> pursTypeName t <> " " <> u <> ")"

pursEq :: Type -> String
pursEq t@(NamedType _)       = pursNominalEq t
pursEq (AppliedType t u)     = "(" <> pursEq t <> " " <> pursEq u <> ")"
pursEq t@(PrimType BoolType)  = pursNominalEq t
pursEq t@(PrimType I32Type)   = pursNominalEq t
pursEq t@(PrimType F64Type)   = pursNominalEq t
pursEq t@(PrimType TextType)  = pursNominalEq t
pursEq (PrimType ArrayType)   = "TDLSUPPORT.eqArray"
pursEq t@(PrimType BytesType) = pursNominalEq t
pursEq (ProductType ts) =
  "(\\tdl__a tdl__b -> " <> foldr (\a b -> a <> " TDLSUPPORT.&& " <> b) "true" entries <> ")"
  where entries = map entry ts
        entry (k /\ t) = "(" <> pursEq t <> " tdl__a." <> k <> " tdl__b." <> k <> ")"
pursEq (SumType ts) =
  "(\\tdl__a tdl__b -> case tdl__a, tdl__b of\n"
  <> indent (fold (Array.mapWithIndex entry ts)) <> "\n"
  <> "  _, _ -> false)"
  where entry i (_ /\ t) = path i "tdl__c" <> ", " <> path i "tdl__d"
                           <> " -> " <> pursEq t <> " tdl__c tdl__d\n"
        path n v | n <= 0    = "TDLSUPPORT.Left " <> v
                 | otherwise = "TDLSUPPORT.Right (" <> path (n - 1) v <> ")"

pursNominalEq :: Type -> String
pursNominalEq t = "(TDLSUPPORT.eq :: " <> n <> " -> " <> n <> " -> Boolean)"
  where n = pursTypeName t

pursSerialize :: Type -> String
pursSerialize (NamedType n) = "intermediateFrom" <> n
pursSerialize (AppliedType t u) = "(" <> pursSerialize t <> " " <> pursSerialize u <> ")"
pursSerialize (PrimType BoolType)  = "TDLSUPPORT.fromBool"
pursSerialize (PrimType I32Type)   = "TDLSUPPORT.fromI32"
pursSerialize (PrimType F64Type)   = "TDLSUPPORT.fromF64"
pursSerialize (PrimType TextType)  = "TDLSUPPORT.fromText"
pursSerialize (PrimType ArrayType) = "TDLSUPPORT.fromArray"
pursSerialize (PrimType BytesType) = "TDLSUPPORT.fromBytes"
pursSerialize (ProductType ts) =
  "(\\tdl__r -> TDLSUPPORT.fromProduct [" <> String.joinWith ", " entries <> "])"
  where entries = map (\(k /\ t) -> pursSerialize t <> " tdl__r." <> k) ts
pursSerialize (SumType ts) = go 0 (List.fromFoldable ts)
  where go _ Nil = "TDLSUPPORT.absurd"
        go n ((_ /\ head) : tail) =
          "(TDLSUPPORT.either "
          <> "(TDLSUPPORT.fromVariant " <> show n <> " " <> pursSerialize head <> ") "
          <> go (n + 1) tail
          <> ")"

pursDeserialize :: Type -> String
pursDeserialize (NamedType n) = "intermediateTo" <> n
pursDeserialize (AppliedType t u) = "(" <> pursDeserialize t <> " " <> pursDeserialize u <> ")"
pursDeserialize (PrimType BoolType)  = "TDLSUPPORT.toBool"
pursDeserialize (PrimType I32Type)   = "TDLSUPPORT.toI32"
pursDeserialize (PrimType F64Type)   = "TDLSUPPORT.toF64"
pursDeserialize (PrimType TextType)  = "TDLSUPPORT.toText"
pursDeserialize (PrimType ArrayType) = "TDLSUPPORT.toArray"
pursDeserialize (PrimType BytesType) = "TDLSUPPORT.toBytes"
pursDeserialize (ProductType ts) =
  "(\\tdl__r -> "
  <> "TDLSUPPORT.toProduct " <> show (Array.length ts) <> " tdl__r"
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
  <> " TDLSUPPORT.toSum tdl__r"
  <> " TDLSUPPORT.>>= case _ of\n" <> fold (Array.mapWithIndex entry ts)
  <> "  {d: _} -> TDLSUPPORT.Left " <> show "Sum discriminator was out of bounds."
  <> ")"
  where entry i (_ /\ t) =
          "  {d: " <> show i <> ", x: tdl__x} -> " <> path i <> " TDLSUPPORT.<$>\n"
          <> indent (indent (pursDeserialize t)) <> " tdl__x\n"
        path n | n <= 0    = "TDLSUPPORT.Left"
               | otherwise = "TDLSUPPORT.Right TDLSUPPORT.<<< " <> path (n - 1)

pursModule :: Module -> String
pursModule (Module n _ m) =
     "module " <> n <> " where\n"
  <> "import TDL.Support as TDLSUPPORT\n"
  <> "import TDL.Intermediate as TDLSUPPORT\n"
  <> foldMap pursDeclaration m

pursDeclaration :: Declaration -> String
pursDeclaration (TypeDeclaration n _ k t) =
  case etaExpandType k t of
    {params, type: t'} ->
      let params' = map (\(p /\ k) -> " (" <> p <> " :: " <> pursKindName k <> ")") params in
         "newtype " <> n <> fold params' <> " = " <> n <> " " <> pursTypeName t' <> "\n"
      <> (if k == SeriKind then eqInstance          else "")
      <> (if k == SeriKind then serializeFunction   else "")
      <> (if k == SeriKind then deserializeFunction else "")
  where
    eqInstance =
         "instance eq" <> n <> " :: TDLSUPPORT.Eq " <> n <> " where\n"
      <> "  eq (" <> n <> " tdl__a) (" <> n <> " tdl__b) =\n"
      <> indent (indent ("(" <> pursEq t <> ") tdl__a tdl__b")) <> "\n"

    serializeFunction =
         "intermediateFrom" <> n <> " :: " <> n <> " -> TDLSUPPORT.Intermediate\n"
      <> "intermediateFrom" <> n <> " (" <> n <> " tdl__a) =\n"
      <> "  " <> pursSerialize t <> " tdl__a\n"

    deserializeFunction =
         "intermediateTo" <> n
      <> " :: TDLSUPPORT.Intermediate -> TDLSUPPORT.Either String " <> n <> "\n"
      <> "intermediateTo" <> n <> " =\n"
      <> indent (pursDeserialize t) <> "\n"
      <> "  TDLSUPPORT.>>> TDLSUPPORT.map " <> n <> "\n"

indent :: String -> String
indent =
  String.split (String.Pattern "\n")
  >>> map ("  " <> _)
  >>> String.joinWith "\n"
