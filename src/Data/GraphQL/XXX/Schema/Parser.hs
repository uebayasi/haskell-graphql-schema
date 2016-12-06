module Data.GraphQL.XXX.Schema.Parser
  ( graphQLStatements
  ) where

import           Data.GraphQL.XXX.Schema.AST
import           Data.GraphQL.XXX.Schema.Lexer
import           Data.GraphQL.XXX.Schema.Token
import           Text.Parsec
import           Text.Parsec.String

graphQLStatements :: Parser [GraphQLStatement]
graphQLStatements = statements statement
  where
    statement
      =   enumDefinition
      <|> inputDefinition
      <|> interfaceDefinition
      <|> objectDefinition
      <|> scalarDefinition
      <|> unionDefinition

-- Enum

enumDefinition :: Parser GraphQLStatement
enumDefinition = EnumDefinition <$> typeDecl "enum" <*> symbols
  where
    symbols = braces enumSymbols

    enumSymbols :: Parser GraphQLEnumNames
    enumSymbols = sepEndBy1 enumName spaces

-- Input

inputDefinition :: Parser GraphQLStatement
inputDefinition = InputDefinition <$> typeDecl "input" <*> fields
  where
    fields = braces objectFields

-- Interface

interfaceDefinition :: Parser GraphQLStatement
interfaceDefinition = InterfaceDefinition <$> typeDecl "interface" <*> fields
  where
    fields = braces objectFields

-- Object

objectDefinition :: Parser GraphQLStatement
objectDefinition = ObjectDefinition <$> typeDecl "type" <*> ifname <*> fields
  where
    ifname = optionMaybe $ typeDecl "implements"
    fields = braces objectFields

objectFields :: Parser [GraphQLField]
objectFields = sepEndBy1 objectField spaces
  where
    objectField :: Parser GraphQLField
    objectField = GraphQLField <$> fieldName <*> args <* delim ':' <*> graphQlTypeName <*> nonnull
      where
        args = optionList $ parens objectArgs
        nonnull = optionBool $ delim '!'

        objectArgs :: Parser [GraphQLArgument]
        objectArgs = sepEndBy1 objectArg (delim ',')
          where
            objectArg :: Parser GraphQLArgument
            objectArg = GraphQLArgument <$> fieldName <* delim ':' <*> graphQlTypeName

-- Scalar

scalarDefinition :: Parser GraphQLStatement
scalarDefinition = ScalarDefinition <$> typeDecl "scalar"

-- Union

unionDefinition :: Parser GraphQLStatement
unionDefinition = UnionDefinition <$> typeDecl "union" <* delim '=' <*> types
  where
    types = sepBy1 typeName (delim '|')
