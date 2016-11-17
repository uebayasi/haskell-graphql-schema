module Data.GraphQL.Schema
    ( graphQLStatements
    , GraphQLStatement(..)
    , GraphQLType(..)
    , GraphQLName(..)
    ) where

import           Text.ParserCombinators.Parsec

data GraphQLStatement
  = EnumDefinition GraphQLTypeName GraphQLEnumNames
  | InterfaceDefinition GraphQLTypeName GraphQLObjectArguments
  | ObjectDefinition GraphQLTypeName GraphQLTypeName GraphQLObjectArguments
  | ScalarDefinition GraphQLTypeName
  | UnionDefinition GraphQLTypeName GraphQLTypeNames

data GraphQLType
  = GraphQLBoolean
  | GraphQLFloat
  | GraphQLList GraphQLType
  | GraphQLInt
  | GraphQLString
  | GraphQLUserType GraphQLTypeName

data GraphQLName
  = GraphQLEnumName String

type GraphQLTypeName = String
type GraphQLTypeNames = [GraphQLTypeName]
type GraphQLSymbolName = String
type GraphQLEnumNames = [GraphQLName]
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
graphQlTypeName = spaces *> graphQlTypeP <* spaces

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

enumNameP :: Parser GraphQLName
enumNameP = GraphQLEnumName <$> many1 upper

graphQlTypeP :: Parser GraphQLType
graphQlTypeP
  =   graphQlBooleanP
  <|> graphQlFloatP
  <|> graphQlIntP
  <|> graphQlListP
  <|> graphQlStringP
  <|> graphQlUserTypeP

graphQlBooleanP :: Parser GraphQLType
graphQlBooleanP = pure GraphQLBoolean <$> string "Boolean"

graphQlFloatP :: Parser GraphQLType
graphQlFloatP = pure GraphQLFloat <$> string "Float"

graphQlIntP :: Parser GraphQLType
graphQlIntP = pure GraphQLInt <$> string "Int"

graphQlListP :: Parser GraphQLType
graphQlListP = GraphQLList <$> brackets graphQlTypeP

graphQlStringP :: Parser GraphQLType
graphQlStringP = pure GraphQLString <$> string "String"

graphQlUserTypeP :: Parser GraphQLType
graphQlUserTypeP = GraphQLUserType <$> typeNameP
