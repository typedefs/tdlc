module TDL.Syntax
  ( Kind(..)
  , Type(..)
  , PrimType(..)

  , Module(..)
  , Declaration(..)

  , Doc(..)

  , prettyKind
  , prettyType
  ) where

import Data.Foldable (foldMap)
import Data.List (List)
import Data.Monoid (class Monoid)
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude

--------------------------------------------------------------------------------

data Kind
  = SeriKind
  | ArrowKind Kind Kind

data Type
  = NamedType String
  | AppliedType Type Type
  | PrimType PrimType
  | ProductType (Array (Tuple String Type))
  | SumType (Array (Tuple String Type))

data PrimType
  = BoolType
  | I32Type
  | F64Type
  | TextType
  | ArrayType
  | BytesType

derive instance eqKind :: Eq Kind
derive instance eqType :: Eq Type
derive instance eqPrimType :: Eq PrimType

--------------------------------------------------------------------------------

data Module = Module String Doc (List Declaration)

data Declaration
  = TypeDeclaration String Doc Kind Type

--------------------------------------------------------------------------------

newtype Doc = Doc String

derive newtype instance semigroupDoc :: Semigroup Doc
derive newtype instance monoidDoc    :: Monoid    Doc

--------------------------------------------------------------------------------

prettyKind :: Kind -> String
prettyKind SeriKind = "*"
prettyKind (ArrowKind a b) = prettyKind' a <> " -> " <> prettyKind b
  where prettyKind' k@SeriKind        = prettyKind k
        prettyKind' k@(ArrowKind _ _) = "(" <> prettyKind k <> ")"

prettyType :: Type -> String
prettyType (NamedType n) = n
prettyType (AppliedType t u) = prettyType t <> " " <> prettyType' u
  where prettyType' t@(NamedType _)     = prettyType t
        prettyType' t@(AppliedType _ _) = "(" <> prettyType t <> ")"
        prettyType' t@(PrimType _)      = prettyType t
        prettyType' t@(ProductType _)   = prettyType t
        prettyType' t@(SumType _)       = prettyType t
prettyType (PrimType BoolType)  = "bool"
prettyType (PrimType I32Type)   = "i32"
prettyType (PrimType F64Type)   = "f64"
prettyType (PrimType TextType)  = "text"
prettyType (PrimType ArrayType) = "array"
prettyType (PrimType BytesType) = "bytes"
prettyType (ProductType []) = "product { }"
prettyType (SumType     []) = "sum { }"
prettyType (ProductType ts) =
     "product {\n"
  <> foldMap (prettyField >>> indent >>> (_ <> ",\n")) ts
  <> "}"
prettyType (SumType ts) =
     "sum {\n"
  <> foldMap (prettyField >>> indent >>> (_ <> ",\n")) ts
  <> "}"

prettyField :: Tuple String Type -> String
prettyField (l /\ t) = l <> ": " <> prettyType t

indent :: String -> String
indent = String.split (String.Pattern "\n")
         >>> map ("    " <> _)
         >>> String.joinWith "\n"
