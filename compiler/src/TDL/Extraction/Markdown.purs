module TDL.Extraction.Markdown
  ( markdownModule
  , markdownDeclaration
  ) where

import Data.Foldable (foldMap)
import Data.String as String
import Prelude
import TDL.Syntax (Declaration(..), Doc(..), Module(..), prettyKind, prettyType)

markdownModule :: Module -> Doc
markdownModule (Module n (Doc d) m) = Doc header <> body
  where header = "# " <> n <> "\n\n" <> d
        body   = foldMap (markdownDeclaration >>> (Doc "\n" <> _)) m

markdownDeclaration :: Declaration -> Doc
markdownDeclaration (TypeDeclaration n (Doc d) k t) = Doc $
     "## " <> n <> "\n"
  <> "\n"
  <> indent ("type " <> n <> " : " <> prettyKind k <> ";") <> "\n"
  <> indent ("type " <> n <> " = " <> prettyType t <> ";") <> "\n"
  <> "\n" <> d
markdownDeclaration (ServiceDeclaration n (Doc d) f t) = Doc $
     "## " <> n <> "\n"
  <> "\n"
  <> indent ("service " <> n <> " : " <> prettyType f <>
                                " -> " <> prettyType t <> ";") <> "\n"
  <> "\n" <> d

indent :: String -> String
indent = String.split (String.Pattern "\n")
         >>> map ("    " <> _)
         >>> String.joinWith "\n"
