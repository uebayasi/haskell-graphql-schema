module Data.GraphQL.XXX.Schema
  ( graphQLStatements
  , GraphQLStatement(..)
  , GraphQLArgument(..)
  , GraphQLEnumName(..)
  , GraphQLFieldName(..)
  , GraphQLObjectField(..)
  , GraphQLTypeName(..)
  , GraphQLType(..)
  ) where

import           Text.ParserCombinators.Parsec

data GraphQLStatement
  = EnumDefinition GraphQLTypeName GraphQLEnumNames
  | InputDefinition GraphQLTypeName GraphQLObjectArguments
  | InterfaceDefinition GraphQLTypeName GraphQLObjectArguments
  | ObjectDefinition GraphQLTypeName (Maybe GraphQLTypeName) GraphQLObjectArguments
  | ScalarDefinition GraphQLTypeName
  | UnionDefinition GraphQLTypeName GraphQLTypeNames
  deriving (Show)

data GraphQLArgument = GraphQLArgument GraphQLFieldName GraphQLType deriving (Show)
data GraphQLEnumName = GraphQLEnumName String deriving (Show)
data GraphQLFieldName = GraphQLFieldName String deriving (Show)
data GraphQLObjectField = GraphQLObjectField GraphQLFieldName GraphQLArguments GraphQLType Bool deriving (Show)
data GraphQLTypeName = GraphQLTypeName String deriving (Show)

type GraphQLArguments = [GraphQLArgument]
type GraphQLEnumNames = [GraphQLEnumName]
type GraphQLObjectArguments = [GraphQLObjectField]
type GraphQLTypeNames = [GraphQLTypeName]

data GraphQLType
  = GraphQLBoolean
  | GraphQLFloat
  | GraphQLList GraphQLType
  | GraphQLID
  | GraphQLInt
  | GraphQLString
  | GraphQLUserType GraphQLTypeName
  deriving (Show)

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
    itypes = braces objectTypes

-- Interface

interfaceDefinition :: Parser GraphQLStatement
interfaceDefinition = InterfaceDefinition <$> name <*> itypes
  where
    name = keyword "interface" typeName
    itypes = braces objectTypes

-- Object

objectDefinition :: Parser GraphQLStatement
objectDefinition = ObjectDefinition <$> name <*> ifname <*> otypes
  where
    name = keyword "type" typeName
    ifname = optionMaybe $ keyword "implements" typeName
    otypes = braces objectTypes

objectTypes :: Parser GraphQLObjectArguments
objectTypes = sepEndBy1 objectType spaces

objectType :: Parser GraphQLObjectField
objectType = GraphQLObjectField <$> name <*> args <*> otype <*> nonnull
  where
    name = fieldName
    args = optionList $ parens objectArgs
    otype = delim ':' *> graphQlTypeName
    nonnull = optionBool $ delim '!'

objectArgs :: Parser GraphQLArguments
objectArgs = sepEndBy1 objectArg (delim ',')

objectArg :: Parser GraphQLArgument
objectArg = GraphQLArgument <$> name <*> otype
  where
    name = fieldName
    otype = delim ':' *> graphQlTypeName

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

enumName :: Parser GraphQLEnumName
enumName = spaces *> enumNameP <* spaces

fieldName :: Parser GraphQLFieldName
fieldName = spaces *> fieldNameP <* spaces

typeName :: Parser GraphQLTypeName
typeName = spaces *> typeNameP <* spaces

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

optionList :: Parser [a] -> Parser [a]
optionList p = option [] p

optionBool :: Parser a -> Parser Bool
optionBool p = option False (p *> pure True)

-- Patterns (no "spaces"!)

enumNameP :: Parser GraphQLEnumName
enumNameP = GraphQLEnumName <$> many1 upper

fieldNameP :: Parser GraphQLFieldName
fieldNameP = GraphQLFieldName <$> ((:) <$> lower <*> many alphaNum)

typeNameP :: Parser GraphQLTypeName
typeNameP = GraphQLTypeName <$> ((:) <$> upper <*> many alphaNum)

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