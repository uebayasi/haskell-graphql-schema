module Data.GraphQL.Schema
    ( graphQLStatements
    , GraphQLStatement(..)
    , GraphQLType(..)
    , GraphQLName(..)
    ) where

import           Text.ParserCombinators.Parsec

data GraphQLStatement
  = EnumDefinition GraphQLName GraphQLEnumNames
  | InputDefinition GraphQLName GraphQLObjectArguments
  | InterfaceDefinition GraphQLName GraphQLObjectArguments
  | ObjectDefinition GraphQLName (Maybe GraphQLName) GraphQLObjectArguments
  | ScalarDefinition GraphQLName
  | UnionDefinition GraphQLName GraphQLTypeNames

data GraphQLType
  = GraphQLBoolean
  | GraphQLFloat
  | GraphQLList GraphQLType
  | GraphQLID
  | GraphQLInt
  | GraphQLString
  | GraphQLUserType GraphQLName

data GraphQLName
  = GraphQLEnumName String
  | GraphQLSymbolName String
  | GraphQLTypeName String

type GraphQLTypeNames = [GraphQLName]
type GraphQLEnumNames = [GraphQLName]
type GraphQLArgument = (GraphQLName, GraphQLType)
type GraphQLArguments = [GraphQLArgument]
type GraphQLObjectArgument = (GraphQLName, GraphQLArguments, GraphQLType, Bool)
type GraphQLObjectArguments = [GraphQLObjectArgument]

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
enumDefinition = EnumDefinition <$> name <*> symbols
  where
    name = keyword "enum" typeName
    symbols = braces enumSymbols

enumSymbols :: Parser GraphQLEnumNames
enumSymbols = sepEndBy1 enumName spaces

-- Input

inputDefinition :: Parser GraphQLStatement
inputDefinition = InputDefinition <$> name <*> itypes
  where
    name = keyword "input" typeName
    args = option [] (parens objectArgs)
    itypes = braces objectTypes

-- Interface

interfaceDefinition :: Parser GraphQLStatement
interfaceDefinition = InterfaceDefinition <$> name <*> itypes
  where
    name = keyword "interface" typeName
    args = option [] (parens objectArgs)
    itypes = braces objectTypes

-- Object

objectDefinition :: Parser GraphQLStatement
objectDefinition = ObjectDefinition <$> name <*> ifname <*> otypes
  where
    name = keyword "type" typeName
    ifname = optionMaybe (keyword "implements" typeName)
    otypes = braces objectTypes

objectTypes :: Parser GraphQLObjectArguments
objectTypes = sepEndBy1 objectType spaces

objectType :: Parser GraphQLObjectArgument
objectType = (,,,) <$> name <*> args <*> otype <*> nonnull
  where
    name = symbolName
    args = option [] (parens objectArgs)
    otype = delim ':' *> graphQlTypeName
    nonnull = option False (delim '!' *> pure True)

objectArgs :: Parser GraphQLArguments
objectArgs = sepEndBy1 objectArg (delim ',')

objectArg :: Parser GraphQLArgument
objectArg = (,) <$> name <*> graphQlTypeName
  where
    name = symbolName <* delim ':'

-- Scalar

scalarDefinition :: Parser GraphQLStatement
scalarDefinition = ScalarDefinition <$> name
  where
    name = keyword "scalar" typeName

-- Union

unionDefinition :: Parser GraphQLStatement
unionDefinition = UnionDefinition <$> name <*> utypes
  where
    name = keyword "union" typeName <* delim '='
    utypes = sepBy1 typeName (delim '|')

-- Common

typeName :: Parser GraphQLName
typeName = spaces *> typeNameP <* spaces

symbolName :: Parser GraphQLName
symbolName = spaces *> symbolNameP <* spaces

enumName :: Parser GraphQLName
enumName = spaces *> enumNameP <* spaces

graphQlTypeName :: Parser GraphQLType
graphQlTypeName = spaces *> graphQlTypeP <* spaces

statements :: Parser a -> Parser [a]
statements s = spaces *> (many s) <* spaces

braces :: Parser a -> Parser a
braces = between (delim '{') (delim '}')

brackets :: Parser a -> Parser a
brackets = between (delim '[') (delim ']')

parens :: Parser a -> Parser a
parens = between (delim '(') (delim ')')

keyword :: String -> Parser a -> Parser a
keyword s p = spaces *> string s *> spaces *> p

delim :: Char -> Parser ()
delim c = spaces *> char c *> spaces

-- Patterns (no "spaces"!)

typeNameP :: Parser GraphQLName
typeNameP = GraphQLTypeName <$> ((:) <$> upper <*> many alphaNum)

symbolNameP :: Parser GraphQLName
symbolNameP = GraphQLSymbolName <$> ((:) <$> lower <*> many alphaNum)

enumNameP :: Parser GraphQLName
enumNameP = GraphQLEnumName <$> many1 upper

graphQlTypeP :: Parser GraphQLType
graphQlTypeP
  =   graphQlBooleanP
  <|> graphQlFloatP
  <|> graphQlIDP
  <|> graphQlIntP
  <|> graphQlListP
  <|> graphQlStringP
  <|> graphQlUserTypeP

graphQlBooleanP :: Parser GraphQLType
graphQlBooleanP = (try $ pure GraphQLBoolean <$> string "Boolean") <?> "Boolean"

graphQlFloatP :: Parser GraphQLType
graphQlFloatP = (try $ pure GraphQLFloat <$> string "Float") <?> "Float"

graphQlIDP :: Parser GraphQLType
graphQlIDP = (try $ pure GraphQLID <$> string "ID") <?> "ID"

graphQlIntP :: Parser GraphQLType
graphQlIntP = (try $ pure GraphQLInt <$> string "Int") <?> "Int"

graphQlListP :: Parser GraphQLType
graphQlListP = (try $ GraphQLList <$> brackets graphQlTypeP) <?> "List"

graphQlStringP :: Parser GraphQLType
graphQlStringP = (try $ pure GraphQLString <$> string "String") <?> "String"

graphQlUserTypeP :: Parser GraphQLType
graphQlUserTypeP = (try $ GraphQLUserType <$> typeNameP) <?> "User-type"
