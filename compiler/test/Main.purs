module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.List ((:), List(Nil))
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Prelude
import Test.Assert (ASSERT, assert)
import TDL.Check (inferKind, inferModule, runCheck)
import TDL.Extraction.PureScript (pursModule, pursSerialize, pursTypeName)
import TDL.Syntax (Declaration(..), Kind(..), Type(..))

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = do
  testKind IntType SeriKind
  testKind (ProductType []) SeriKind
  testKind (ProductType ["x" /\ IntType, "y" /\ IntType]) SeriKind
  testKind (SumType []) SeriKind
  testKind (SumType ["x" /\ IntType, "y" /\ IntType]) SeriKind
  testKind (FuncType IntType IntType) TypeKind
  testKind (SumType ["x" /\ IntType, "y" /\ FuncType IntType IntType]) TypeKind

  testModule $ Nil
  testModule $ TypeDeclaration "T" TypeKind IntType : Nil
  testModule $ TypeDeclaration "T" SeriKind IntType
             : TypeDeclaration "U" SeriKind (NamedType "T")
             : Nil

  exampleType $ IntType
  exampleType $ ProductType []
  exampleType $ ProductType ["x" /\ IntType]
  exampleType $ ProductType ["x" /\ IntType, "y" /\ ProductType []]

  exampleModule $ Nil
  exampleModule $ TypeDeclaration "T" TypeKind IntType : Nil
  exampleModule $ TypeDeclaration "T" SeriKind IntType
                : TypeDeclaration "U" SeriKind (NamedType "T")
                : Nil
  exampleModule $ TypeDeclaration "T" SeriKind IntType
                : TypeDeclaration "U" SeriKind (ProductType ["x" /\ IntType, "y" /\ NamedType "T"])
                : Nil
  exampleModule $ TypeDeclaration "T" SeriKind IntType
                : TypeDeclaration "U" SeriKind (ProductType ["x" /\ IntType, "y" /\ ProductType []])
                : Nil

  where
    testKind t k = assert $ runCheck (inferKind t) == Right k
    testModule m = assert $ runCheck (inferModule m) == Right unit
    exampleType t = do
      log $ pursTypeName t
      log $ unsafePartial pursSerialize t
    exampleModule m = log $ unsafePartial pursModule m
