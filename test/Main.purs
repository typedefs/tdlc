module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Prelude
import Test.Assert (ASSERT, assert)
import TDL.Check (inferKind, runCheck)
import TDL.Syntax (Kind(..), Type(..))

main :: forall eff. Eff (assert :: ASSERT | eff) Unit
main = do
  testKind IntType SeriKind
  testKind (ProductType []) SeriKind
  testKind (ProductType ["x" /\ IntType, "y" /\ IntType]) SeriKind
  testKind (SumType []) SeriKind
  testKind (SumType ["x" /\ IntType, "y" /\ IntType]) SeriKind
  testKind (FuncType IntType IntType) TypeKind
  testKind (SumType ["x" /\ IntType, "y" /\ FuncType IntType IntType]) TypeKind

  where
    testKind t k = assert $ runCheck (inferKind t) == Right k
