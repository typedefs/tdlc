module TDL.Extraction.PureScript
  ( pursTypeName
  , pursSerialize
  , pursModule
  , pursDeclaration
  ) where

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.String as String
import Data.Tuple (fst)
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
pursSerialize IntType = "serializeInt"
pursSerialize (ProductType ts) =
    "(\\r -> serializeProduct [" <> String.joinWith ", " entries <> "])"
    where entries = map (\(k /\ t) -> pursSerialize t <> " r." <> k) ts
pursSerialize (SumType ts) = "TODO"

-- | This function may throw on ill-typed inputs.
pursDeserialize :: Partial => Type -> String
pursDeserialize (NamedType n) = "deserializeNamed" <> n
pursDeserialize IntType = "deserializeInt"
pursDeserialize (ProductType ts) =
    "(\\r -> do\n"
    <> "  r' <- deserializeProduct " <> show (Array.length ts) <> " r\n"
    <> String.joinWith "\n" (map indent entries) <> "\n"
    <> "  pure {" <> String.joinWith ", " (map fst ts) <> "}"
    <> ")"
    where entries = ts # Array.mapWithIndex \i (k /\ t) ->
            "" <> k <> " <-\n"
            <> indent (pursDeserialize t <> " (r' `unsafePartial unsafeIndex` " <> show i <> ")")
pursDeserialize (SumType ts) = "TODO"

-- | This function may throw on ill-typed inputs.
pursModule :: Partial => Module -> String
pursModule = foldMap pursDeclaration

-- | This function may throw on ill-typed inputs.
pursDeclaration :: Partial => Declaration -> String
pursDeclaration (TypeDeclaration n t) =
       "newtype " <> n <> " = " <> n <> " " <> pursTypeName t <> "\n"
    <> "serializeNamed" <> n <> " :: " <> n <> " -> Json\n"
    <> "serializeNamed" <> n <> " (" <> n <> " x) =\n"
    <> "  " <> pursSerialize t <> " x\n"
    <> "deserializeNamed" <> n <> " :: Json -> Either String " <> n <> "\n"
    <> "deserializeNamed" <> n <> " =\n"
    <> indent (pursDeserialize t) <> "\n"
    <> "  >>> map " <> n <> "\n"

indent :: String -> String
indent =
    String.split (String.Pattern "\n")
    >>> map ("  " <> _)
    >>> String.joinWith "\n"
