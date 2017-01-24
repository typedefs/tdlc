module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Prelude
import Test.Assert (ASSERT, assert)
import TDL.Check (inferKind, runCheck)
import TDL.Extraction.PureScript (pursSerialize, pursTypeName)
import TDL.Syntax (Kind(..), Type(..))

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = do
  testKind IntType SeriKind
  testKind (ProductType []) SeriKind
  testKind (ProductType ["x" /\ IntType, "y" /\ IntType]) SeriKind
  testKind (SumType []) SeriKind
  testKind (SumType ["x" /\ IntType, "y" /\ IntType]) SeriKind
  testKind (FuncType IntType IntType) TypeKind
  testKind (SumType ["x" /\ IntType, "y" /\ FuncType IntType IntType]) TypeKind

  example $ IntType
  example $ ProductType []
  example $ ProductType ["x" /\ IntType]
  example $ ProductType ["x" /\ IntType, "y" /\ ProductType []]

  where
    testKind t k = assert $ runCheck (inferKind t) == Right k
    example t = do
      log $ pursTypeName t
      log $ unsafePartial pursSerialize t
