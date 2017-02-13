module TDL.Check
  ( Check
  , Environment
  , Error(..)
  , prettyError
  , runCheck

  , inferKind

  , checkSumAppearance

  , inferModule
  , inferDeclaration
  , checkDeclaration
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
import TDL.Syntax (Declaration(..), Kind(..), Module(..), PrimType(..), Type(..))

--------------------------------------------------------------------------------

type Check = ReaderT Environment (Except Error)

type Environment = Map String Kind

data Error
  = NameError String
  | KindError Kind Kind
  | ApplyError
  | SumAppearanceError

derive instance eqError :: Eq Error

prettyError :: Error -> String
prettyError (NameError n) = "'" <> n <> "' is not defined."
prettyError (KindError a b) = "'" <> f a <> "' /= '" <> f b <> "'."
  where f SeriKind = "*"
        f (ArrowKind k k') = "(" <> f k <> " -> " <> f k' <> ")"
prettyError (ApplyError) = "applied type was not a type constructor."
prettyError (SumAppearanceError) =
  "non-void sum type appeared as part of other type."

runCheck :: forall a. Check a -> Either Error a
runCheck m = runExcept (runReaderT m Map.empty)

--------------------------------------------------------------------------------

inferKind :: Type -> Check Kind
inferKind (NamedType n) =
  Reader.asks (Map.lookup n)
  >>= maybe (throwError (NameError n)) pure
inferKind (AppliedType t u) =
  inferKind t >>= case _ of
    ArrowKind kt1 kt2 -> do
      ku <- inferKind u
      assertKind kt1 ku
      pure kt2
    _ -> throwError ApplyError
inferKind (PrimType BoolType)  = pure SeriKind
inferKind (PrimType I32Type)   = pure SeriKind
inferKind (PrimType F64Type)   = pure SeriKind
inferKind (PrimType TextType)  = pure SeriKind
inferKind (PrimType ArrayType) = pure (ArrowKind SeriKind SeriKind)
inferKind (PrimType BytesType) = pure SeriKind
inferKind (ProductType ts) = SeriKind <$ traverse_ (assertKind SeriKind <=< inferKind <<< snd) ts
inferKind (SumType     ts) = SeriKind <$ traverse_ (assertKind SeriKind <=< inferKind <<< snd) ts

assertKind :: Kind -> Kind -> Check Unit
assertKind k k' = when (k /= k') $ throwError (KindError k k')

--------------------------------------------------------------------------------

-- | Check that non-void sum types only appear at the top level.
checkSumAppearance :: Type -> Check Unit
checkSumAppearance = outer
  where
    outer (SumType ts) = traverse_ (inner <<< snd) ts
    outer t = inner t

    inner (NamedType _)     = pure unit
    inner (AppliedType t u) = inner t *> inner u
    inner (PrimType _)      = pure unit
    inner (ProductType ts)  = traverse_ (inner <<< snd) ts
    inner (SumType [])      = pure unit
    inner (SumType _)       = throwError SumAppearanceError

--------------------------------------------------------------------------------

inferModule :: Module -> Check Unit
inferModule (Module _ _ ds) = do
  mappings <- List.foldM (\a b -> Map.union a <$> inferDeclaration b) Map.empty ds
  Reader.local (Map.union mappings) $
    traverse_ checkDeclaration ds

inferDeclaration :: Declaration -> Check Environment
inferDeclaration (TypeDeclaration n _ k _) = pure $ Map.singleton n k

checkDeclaration :: Declaration -> Check Unit
checkDeclaration (TypeDeclaration _ _ k t) = do
  checkSumAppearance t
  k' <- inferKind t
  assertKind k k'
