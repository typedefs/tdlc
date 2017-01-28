module TDL.Syntax
  ( Kind(..)
  , Type(..)
  , PrimType(..)

  , subkind
  , KindLUB(..)

  , Module
  , Declaration(..)
  ) where

import Data.List (List)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Prelude

--------------------------------------------------------------------------------

data Kind
  = TypeKind
  | SeriKind

data Type
  = NamedType String
  | PrimType PrimType
  | ProductType (Array (Tuple String Type))
  | SumType (Array (Tuple String Type))
  | FuncType Type Type

data PrimType
  = I32Type
  | F64Type
  | TextType

derive instance eqKind :: Eq Kind
derive instance eqType :: Eq Type
derive instance eqPrimType :: Eq PrimType

--------------------------------------------------------------------------------

subkind :: Kind -> Kind -> Boolean
subkind TypeKind TypeKind = true
subkind TypeKind SeriKind = false
subkind SeriKind TypeKind = true
subkind SeriKind SeriKind = true

newtype KindLUB = KindLUB Kind

derive instance newtypeKindLUB :: Newtype KindLUB _

instance semigroupKindLUB :: Semigroup KindLUB where
  append (KindLUB TypeKind) _ = KindLUB TypeKind
  append _ (KindLUB TypeKind) = KindLUB TypeKind
  append (KindLUB SeriKind) (KindLUB SeriKind) = KindLUB SeriKind

instance monoidKindLUB :: Monoid KindLUB where
  mempty = KindLUB SeriKind

--------------------------------------------------------------------------------

type Module = List Declaration

data Declaration
  = TypeDeclaration String Kind Type
