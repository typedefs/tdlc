module TDL.Check
  ( Check
  , Environment
  , Error(..)
  , prettyError
  , runCheck

  , inferKind

  , inferModule
  , inferDeclaration
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader.Class as Reader
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import Prelude
import TDL.Syntax (Declaration(..), Kind(..), Module, Type(..))

--------------------------------------------------------------------------------

type Check = ReaderT Environment (Except Error)

type Environment = Map String Kind

data Error
  = NameError String
  | KindError Kind Kind

derive instance eqError :: Eq Error

prettyError :: Error -> String
prettyError (NameError n) = "'" <> n <> "' is not defined."
prettyError (KindError a b) = f a <> " /= " <> f b <> "."
  where f SeriKind = "'*'"

runCheck :: forall a. Check a -> Either Error a
runCheck m = runExcept (runReaderT m Map.empty)

--------------------------------------------------------------------------------

inferKind :: Type -> Check Kind
inferKind (NamedType n) =
  Reader.asks (Map.lookup n)
  >>= maybe (throwError (NameError n)) pure
inferKind (PrimType _) = pure SeriKind
inferKind (ProductType ts) = SeriKind <$ traverse_ (assertKind SeriKind <=< inferKind <<< snd) ts
inferKind (SumType     ts) = SeriKind <$ traverse_ (assertKind SeriKind <=< inferKind <<< snd) ts

assertKind :: Kind -> Kind -> Check Unit
assertKind k k' = when (k /= k') $ throwError (KindError k k')

--------------------------------------------------------------------------------

inferModule :: Module -> Check Unit
inferModule ds = do
  mappings <- List.foldM (\a b -> Map.union a <$> inferDeclaration b) Map.empty ds
  Reader.local (Map.union mappings) $
    traverse_ checkDeclaration ds

inferDeclaration :: Declaration -> Check Environment
inferDeclaration (TypeDeclaration n k _) = pure $ Map.singleton n k

checkDeclaration :: Declaration -> Check Unit
checkDeclaration (TypeDeclaration _ k t) = do
  k' <- inferKind t
  assertKind k k'
