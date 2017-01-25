module TDL.Extraction.PureScript
  ( pursTypeName
  , pursSerialize
  , pursModule
  , pursDeclaration
  ) where

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

pursSerialize :: Partial => Type -> String
pursSerialize (NamedType n) = "serializeNamed" <> n
pursSerialize IntType = "serializeInt"
pursSerialize (ProductType ts) =
    "(\\r -> serializeProduct [" <> String.joinWith ", " entries <> "])"
    where entries = map (\(k /\ t) -> pursSerialize t <> " r." <> k) ts
pursSerialize (SumType ts) = "TODO"

pursModule :: Partial => Module -> String
pursModule = foldMap pursDeclaration

pursDeclaration :: Partial => Declaration -> String
pursDeclaration (TypeDeclaration n t) =
       "newtype " <> n <> " = " <> n <> " " <> pursTypeName t <> "\n"
    <> "serializeNamed" <> n <> " :: " <> n <> " -> Json\n"
    <> "serializeNamed" <> n <> " (" <> n <> " x) =\n"
    <> "  " <> pursSerialize t <> " x\n"
