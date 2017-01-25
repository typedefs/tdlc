module TDL.Extraction.PureScript
  ( pursTypeName
  , pursSerialize
  , pursModule
  , pursDeclaration
  ) where

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Prelude
import TDL.Syntax (Declaration(..), Module, Type(..))

pursTypeName :: Type -> String
pursTypeName (NamedType n) = n
pursTypeName IntType = "Int"
pursTypeName (ProductType ts) = "{" <> String.joinWith ", " entries <> "}"
  where entries = map (\(k /\ t) -> k <> " :: " <> pursTypeName t) ts
pursTypeName (SumType ts) = "TODO"
pursTypeName (FuncType a b) = "(" <> pursTypeName a <> " -> " <> pursTypeName b <> ")"

-- | This function may throw on ill-typed inputs.
pursSerialize :: Partial => Type -> String
pursSerialize (NamedType n) = "serializeNamed" <> n
pursSerialize IntType = "TDLSUPPORT.serializeInt"
pursSerialize (ProductType ts) =
  "(\\tdl__r -> TDLSUPPORT.serializeProduct [" <> String.joinWith ", " entries <> "])"
  where entries = map (\(k /\ t) -> pursSerialize t <> " tdl__r." <> k) ts
pursSerialize (SumType ts) = "TODO"

-- | This function may throw on ill-typed inputs.
pursDeserialize :: Partial => Type -> String
pursDeserialize (NamedType n) = "deserializeNamed" <> n
pursDeserialize IntType = "TDLSUPPORT.deserializeInt"
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
pursDeserialize (SumType ts) = "TODO"

-- | This function may throw on ill-typed inputs.
pursModule :: Partial => Module -> String
pursModule m =
     "import TDL.Support as TDLSUPPORT\n"
  <> foldMap pursDeclaration m

-- | This function may throw on ill-typed inputs.
pursDeclaration :: Partial => Declaration -> String
pursDeclaration (TypeDeclaration n t) =
     "newtype " <> n <> " = " <> n <> " " <> pursTypeName t <> "\n"
  <> "serializeNamed" <> n <> " :: " <> n <> " -> TDLSUPPORT.Json\n"
  <> "serializeNamed" <> n <> " (" <> n <> " x) =\n"
  <> "  " <> pursSerialize t <> " x\n"
  <> "deserializeNamed" <> n <> " :: TDLSUPPORT.Json -> TDLSUPPORT.Either String " <> n <> "\n"
  <> "deserializeNamed" <> n <> " =\n"
  <> indent (pursDeserialize t) <> "\n"
  <> "  TDLSUPPORT.>>> TDLSUPPORT.map " <> n <> "\n"

indent :: String -> String
indent =
  String.split (String.Pattern "\n")
  >>> map ("  " <> _)
  >>> String.joinWith "\n"
