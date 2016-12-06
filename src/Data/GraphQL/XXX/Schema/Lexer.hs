module Data.GraphQL.XXX.Schema.Lexer
  ( enumName
  , fieldName
  , typeName
  , graphQlTypeName
  , statements
  , typeDecl
  , braces
  , parens
  , delim
  , optionList
  , optionBool
  , dump
  ) where

import           Data.GraphQL.XXX.Schema.AST
import           Data.GraphQL.XXX.Schema.Token
import           Debug.Trace                   (trace)
import           Text.Parsec
import           Text.Parsec.String

-- Common

enumName :: Parser GraphQLEnumName
enumName = spaces *> enumNameP <* spaces

fieldName :: Parser GraphQLFieldName
fieldName = spaces *> fieldNameP <* spaces

typeName :: Parser GraphQLTypeName
typeName = spaces *> typeNameP <* spaces

graphQlTypeName :: Parser GraphQLType
graphQlTypeName = spaces *> graphQlTypeP <* spaces

statements :: Parser a -> Parser [a]
statements s = spaces *> many s <* spaces

typeDecl :: String -> Parser GraphQLTypeName
typeDecl kw = spaces *> try (string kw) *> spaces *> typeName

braces :: Parser a -> Parser a
braces = between (delim '{') (delim '}')

parens :: Parser a -> Parser a
parens = between (delim '(') (delim ')')

delim :: Char -> Parser ()
delim c = spaces *> char c *> spaces

optionList :: Parser [a] -> Parser [a]
optionList = option []

optionBool :: Parser a -> Parser Bool
optionBool p = option False (p *> pure True)

dump :: Int -> Parser ()
dump n = do
  s <- getParserState
  let msg = show $ take n (stateInput s)
  trace msg $ return ()
