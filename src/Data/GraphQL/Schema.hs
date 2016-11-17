module Data.GraphQL.Schema
    ( graphQLStatements
    , GraphQLStatement(..)
    ) where

import           Text.ParserCombinators.Parsec

data GraphQLStatement
  = EnumDefinition GraphQLTypeName GraphQLEnumNames
  | InterfaceDefinition GraphQLTypeName GraphQLObjectArguments
  | ObjectDefinition GraphQLTypeName GraphQLTypeName GraphQLObjectArguments
  | ScalarDefinition GraphQLTypeName
  | UnionDefinition GraphQLTypeName GraphQLTypeNames

data GraphQLType
  = GraphQLTypeBoolean
  | GraphQLTypeFloat
  | GraphQLTypeList GraphQLType
  | GraphQLTypeInt
  | GraphQLTypeString
  | GraphQLTypeUser GraphQLTypeName

type GraphQLTypeName = String
type GraphQLTypeNames = [GraphQLTypeName]
type GraphQLSymbolName = String
type GraphQLEnumName = String
type GraphQLEnumNames = [GraphQLEnumName]
type GraphQLArgument = (GraphQLSymbolName, GraphQLType)
type GraphQLArguments = [GraphQLArgument]
type GraphQLObjectArgument = (GraphQLSymbolName, GraphQLArguments, GraphQLType, Bool)
type GraphQLObjectArguments = [GraphQLObjectArgument]

graphQLStatements :: Parser [GraphQLStatement]
graphQLStatements = statements statement
  where
    statement
      =   enumDefinition
      <|> interfaceDefinition
      <|> objectDefinition
      <|> scalarDefinition
      <|> unionDefinition

-- Enum

enumDefinition :: Parser GraphQLStatement
enumDefinition = EnumDefinition <$> name <*> symbols
  where
    name = keyword "enum" *> typeName
    symbols = braces enumSymbols

enumSymbols :: Parser GraphQLEnumNames
enumSymbols = sepEndBy1 enumName spaces

-- Interface

interfaceDefinition :: Parser GraphQLStatement
interfaceDefinition = InterfaceDefinition <$> name <*> itypes
  where
    name = keyword "interface" *> typeName
    args = option [] $ parens objectArgs
    itypes = braces objectTypes

-- Object

objectDefinition :: Parser GraphQLStatement
objectDefinition = ObjectDefinition <$> name <*> ifname <*> otypes
  where
    name = keyword "type" *> typeName
    ifname = option [] $ keyword "implements" *> typeName
    otypes = braces objectTypes

objectArgs :: Parser GraphQLArguments
objectArgs = sepEndBy1 objectArg (delim ',')

objectArg :: Parser GraphQLArgument
objectArg = (,) <$> name <*> graphQlTypeName
  where
    name = symbolName <* delim ':'

objectTypes :: Parser GraphQLObjectArguments
objectTypes = sepEndBy1 objectType spaces

objectType :: Parser GraphQLObjectArgument
objectType = (,,,) <$> name <*> args <*> otype <*> nonnull
  where
    name = symbolName
    args = option [] $ parens objectArgs
    otype = delim ':' *> graphQlTypeName
    nonnull = option False $ delim '!' *> pure True

-- Scalar

scalarDefinition :: Parser GraphQLStatement
scalarDefinition = ScalarDefinition <$> name
  where
    name = keyword "scalar" *> typeName

-- Union

unionDefinition :: Parser GraphQLStatement
unionDefinition = UnionDefinition <$> name <*> utypes
  where
    name = keyword "union" *> typeName <* delim '='
    utypes = sepBy1 typeName (delim '|')

-- Common

typeName = spaces *> typeNameP <* spaces
symbolName = spaces *> symbolNameP <* spaces
enumName = spaces *> enumNameP <* spaces
graphQlTypeName = spaces *> graphQlType <* spaces

statements :: Parser a -> Parser [a]
statements s = spaces *> (many s) <* spaces

braces :: Parser a -> Parser a
braces = between (delim '{') (delim '}')

brackets :: Parser a -> Parser a
brackets = between (delim '[') (delim ']')

parens :: Parser a -> Parser a
parens = between (delim '(') (delim ')')

keyword :: String -> Parser ()
keyword s = spaces *> string s *> spaces

delim :: Char -> Parser ()
delim c = spaces *> char c *> spaces

-- Patterns (no "spaces"!)

typeNameP :: Parser GraphQLTypeName
typeNameP = (:) <$> upper <*> many alphaNum

symbolNameP :: Parser GraphQLSymbolName
symbolNameP = (:) <$> lower <*> many alphaNum

enumNameP :: Parser GraphQLEnumName
enumNameP = many1 upper

graphQlType :: Parser GraphQLType
graphQlType
  =   graphQlTypeBoolean
  <|> graphQlTypeFloat
  <|> graphQlTypeInt
  <|> graphQlTypeList
  <|> graphQlTypeString
  <|> graphQlTypeUser

graphQlTypeBoolean :: Parser GraphQLType
graphQlTypeBoolean = pure GraphQLTypeBoolean <$> string "Boolean"

graphQlTypeFloat :: Parser GraphQLType
graphQlTypeFloat = pure GraphQLTypeFloat <$> string "Float"

graphQlTypeInt :: Parser GraphQLType
graphQlTypeInt = pure GraphQLTypeInt <$> string "Int"

graphQlTypeList :: Parser GraphQLType
graphQlTypeList = GraphQLTypeList <$> brackets graphQlType

graphQlTypeString :: Parser GraphQLType
graphQlTypeString = pure GraphQLTypeString <$> string "String"

graphQlTypeUser :: Parser GraphQLType
graphQlTypeUser = GraphQLTypeUser <$> typeNameP
