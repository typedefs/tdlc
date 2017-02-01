module TDL.Parse
  ( parse
  ) where

import Control.Alternative ((<|>))
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Either (Either)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Prelude
import TDL.Syntax (Declaration(..), Kind(..), Module, PrimType(..), Type(..))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.Combinators as PC
import Text.Parsing.StringParser.String as PS

--------------------------------------------------------------------------------

parse :: String -> Either ParseError Module
parse = runParser module_

--------------------------------------------------------------------------------

kind_ :: Parser Kind
kind_ = SeriKind <$ asteriskPunc

type_ :: Parser Type
type_ = pure unit >>= \_ -> type' unit

type' :: Unit -> Parser Type
type' _ = do
  head <-     P.try (NamedType <$> identifier)
          <|> P.try (PrimType <$> primType)
          <|> (ProductType <$> (leftBracePunc   *> fields <* rightBracePunc))
          <|> (SumType     <$> (leftBracketPunc *> fields <* rightBracketPunc))
  pure head
  where fields = Array.fromFoldable <$> (field `PC.sepEndBy` commaPunc)
        field  = (/\) <$> identifier <*> (colonPunc *> type_)

primType :: Parser PrimType
primType =     (I32Type       <$ i32Keyword)
           <|> (F64Type       <$ f64Keyword)
           <|> (TextType      <$ textKeyword)

--------------------------------------------------------------------------------

module_ :: Parser Module
module_ = PC.many declaration <* PS.eof

declaration :: Parser Declaration
declaration = do
  typeKeyword
  name <- identifier
  colonPunc
  kind' <- kind_
  equalsSignPunc
  original <- type_
  semicolonPunc
  pure $ TypeDeclaration name kind' original

--------------------------------------------------------------------------------

lexeme :: forall a. Parser a -> Parser a
lexeme p = blank *> p <* blank
  where blank   = PS.whiteSpace *> PC.many (comment *> PS.whiteSpace)
        comment = void $ PS.string "/*" *> PS.anyChar `PC.manyTill` PS.string "*/"

identifier :: Parser String
identifier = lexeme do
  name <- String.fromCharArray <<< Array.fromFoldable <$> PC.many1 PS.alphaNum
  guard (name `Array.notElem` ["f64", "i32", "text", "type"])
  pure name

f64Keyword :: Parser Unit
f64Keyword = void $ lexeme $ PS.string "f64"

i32Keyword :: Parser Unit
i32Keyword = void $ lexeme $ PS.string "i32"

textKeyword :: Parser Unit
textKeyword = void $ lexeme $ PS.string "text"

typeKeyword :: Parser Unit
typeKeyword = void $ lexeme $ PS.string "type"

asteriskPunc :: Parser Unit
asteriskPunc = void $ lexeme $ PS.char '*'

colonPunc :: Parser Unit
colonPunc = void $ lexeme $ PS.char ':'

commaPunc :: Parser Unit
commaPunc = void $ lexeme $ PS.char ','

equalsSignPunc :: Parser Unit
equalsSignPunc = void $ lexeme $ PS.char '='

leftBracePunc :: Parser Unit
leftBracePunc = void $ lexeme $ PS.char '{'

leftBracketPunc :: Parser Unit
leftBracketPunc = void $ lexeme $ PS.char '['

rightBracePunc :: Parser Unit
rightBracePunc = void $ lexeme $ PS.char '}'

rightBracketPunc :: Parser Unit
rightBracketPunc = void $ lexeme $ PS.char ']'

semicolonPunc :: Parser Unit
semicolonPunc = void $ lexeme $ PS.char ';'
