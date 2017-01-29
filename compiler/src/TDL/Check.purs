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
import Data.Foldable (foldMap)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (ala)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (snd)
import Prelude
import TDL.Syntax (Declaration(..), Kind(..), KindLUB(..), Module, subkind, Type(..))

--------------------------------------------------------------------------------

type Check = ReaderT Environment (Except Error)

type Environment = Map String Kind

data Error
  = NameError String
  | KindError Kind Kind

derive instance eqError :: Eq Error

prettyError :: Error -> String
prettyError (NameError n) = "'" <> n <> "' is not defined."
prettyError (KindError a b) = f a <> " is not a subkind of " <> f b <> "."
  where f TypeKind = "'type'"
        f SeriKind = "'serializable'"

runCheck :: forall a. Check a -> Either Error a
runCheck m = runExcept (runReaderT m Map.empty)

--------------------------------------------------------------------------------

inferKind :: Type -> Check Kind
inferKind (NamedType n) =
  Reader.asks (Map.lookup n)
  >>= maybe (throwError (NameError n)) pure
inferKind (PrimType _) = pure SeriKind
inferKind (ProductType ts) = ala KindLUB foldMap <$> traverse (inferKind <<< snd) ts
inferKind (SumType     ts) = ala KindLUB foldMap <$> traverse (inferKind <<< snd) ts
inferKind (FuncType a b) = TypeKind <$ inferKind a <* inferKind b

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
  when (not (k' `subkind` k)) $
    throwError $ KindError k' k
