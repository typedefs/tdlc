module TDL.LambdaCalculus
  ( etaExpandType
  ) where

import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude
import TDL.Syntax (Kind(..), Type(..))

etaExpandType :: Kind -> Type -> {params :: Array (Tuple String Kind), type :: Type}
etaExpandType = go 0 []
  where go _ ps SeriKind          t = {params: ps, type: t}
        go n ps (ArrowKind k1 k2) t = go (n + 1) (ps <> [p /\ k1]) k2
                                         (AppliedType t (NamedType p))
          where p = "tdl__p" <> show n
