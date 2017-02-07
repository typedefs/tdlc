module TDL.Syntax
  ( Kind(..)
  , Type(..)
  , PrimType(..)

  , Module(..)
  , Declaration(..)
  ) where

import Data.List (List)
import Data.Tuple (Tuple)
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
  = I32Type
  | F64Type
  | TextType
  | ArrayType

derive instance eqKind :: Eq Kind
derive instance eqType :: Eq Type
derive instance eqPrimType :: Eq PrimType

--------------------------------------------------------------------------------

data Module = Module String (List Declaration)

data Declaration
  = TypeDeclaration String Kind Type
