module Data.GraphQL.XXX.Schema
  ( graphQLStatements
  , GraphQLStatement(..)
  , GraphQLArgument(..)
  , GraphQLField(..)
  , GraphQLEnumName(..)
  , GraphQLFieldName(..)
  , GraphQLTypeName(..)
  , GraphQLType(..)
  ) where

import           Text.ParserCombinators.Parsec

data GraphQLStatement
  = EnumDefinition GraphQLTypeName GraphQLEnumNames
  | InputDefinition GraphQLTypeName GraphQLFields
  | InterfaceDefinition GraphQLTypeName GraphQLFields
  | ObjectDefinition GraphQLTypeName (Maybe GraphQLTypeName) GraphQLFields
  | ScalarDefinition GraphQLTypeName
  | UnionDefinition GraphQLTypeName GraphQLTypeNames
  deriving (Show)

data GraphQLArgument = GraphQLArgument GraphQLFieldName GraphQLType deriving (Show)
data GraphQLField = GraphQLField GraphQLFieldName GraphQLArguments GraphQLType Bool deriving (Show)

type GraphQLArguments = [GraphQLArgument]
type GraphQLFields = [GraphQLField]

data GraphQLEnumName = GraphQLEnumName String deriving (Show)
data GraphQLFieldName = GraphQLFieldName String deriving (Show)
data GraphQLTypeName = GraphQLTypeName String deriving (Show)

type GraphQLEnumNames = [GraphQLEnumName]
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
    ifname = optionMaybe $ keyword "implements" typeName
    fields = braces objectFields

objectFields :: Parser GraphQLFields
objectFields = sepEndBy1 objectField spaces
  where
    objectField :: Parser GraphQLField
    objectField = GraphQLField <$> fieldName <*> args <* delim ':' <*> graphQlTypeName <*> nonnull
      where
        args = optionList $ parens objectArgs
        nonnull = optionBool $ delim '!'

        objectArgs :: Parser GraphQLArguments
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

typeDecl :: String -> Parser GraphQLTypeName
typeDecl kw = keyword kw typeName

braces :: Parser a -> Parser a
braces = between (delim '{') (delim '}')

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
graphQlBooleanP = try (pure GraphQLBoolean <$> string "Boolean") <?> "Boolean"

graphQlFloatP :: Parser GraphQLType
graphQlFloatP = try (pure GraphQLFloat <$> string "Float") <?> "Float"

graphQlIDP :: Parser GraphQLType
graphQlIDP = try (pure GraphQLID <$> string "ID") <?> "ID"

graphQlIntP :: Parser GraphQLType
graphQlIntP = try (pure GraphQLInt <$> string "Int") <?> "Int"

graphQlListP :: Parser GraphQLType
graphQlListP = try (GraphQLList <$> between (char '(') (char ')') graphQlTypeP) <?> "List"

graphQlStringP :: Parser GraphQLType
graphQlStringP = try (pure GraphQLString <$> string "String") <?> "String"

graphQlUserTypeP :: Parser GraphQLType
graphQlUserTypeP = try (GraphQLUserType <$> typeNameP) <?> "User-type"
