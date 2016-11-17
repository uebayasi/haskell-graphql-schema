module Main where

import           Lib
import           System.Environment
import           Text.ParserCombinators.Parsec

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readSchema expr)

readSchema :: String -> String
readSchema input = case parse graphQLStatements "GraphQL Schema" input of
  Left err -> "Error: " ++ show err
  Right val -> case val of
    EnumDefinition name symbols:rest ->
      "Enum! name=" ++ name ++ " symbols=" ++ joinNames ',' symbols
    InterfaceDefinition name _:rest ->
      "Interface! name=" ++ name
    ScalarDefinition name:rest ->
      "Scalar! name=" ++ name
    ObjectDefinition name ifname _:rest ->
      "Type! name=" ++ name ++ " ifname=" ++ ifname
    UnionDefinition name utypes:rest ->
      "Union! name=" ++ name ++ " utypes=" ++ joinNames '|' utypes

joinNames :: Char -> [String] -> String
joinNames sep names =
  case names of
    []         -> ""
    first:[]   -> first
    first:rest -> first ++ concatMap ((:) sep) rest

data GraphQLStatement
  = EnumDefinition GraphQLTypeName [GraphQLEnumName]
  | InterfaceDefinition GraphQLTypeName [(GraphQLSymbolName, [(GraphQLSymbolName, GraphQLType)], GraphQLType, Bool)]
  | ObjectDefinition GraphQLTypeName String [(GraphQLSymbolName, [(GraphQLSymbolName, GraphQLType)], GraphQLType, Bool)]
  | ScalarDefinition GraphQLTypeName
  | UnionDefinition GraphQLTypeName [GraphQLTypeName]

data GraphQLType
  = GraphQLTypeBoolean
  | GraphQLTypeFloat
  | GraphQLTypeList GraphQLType
  | GraphQLTypeInt
  | GraphQLTypeString
  | GraphQLTypeUser String

type GraphQLTypeName = String
type GraphQLSymbolName = String
type GraphQLEnumName = String
type GraphQLArgument = (String, GraphQLType)
type GraphQLArguments = [GraphQLArgument]

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

enumSymbols :: Parser [String]
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

objectArgs :: Parser [(String, GraphQLType)]
objectArgs = sepEndBy1 objectArg (delim ',')

objectArg :: Parser (String, GraphQLType)
objectArg = (,) <$> name <*> graphQlTypeName
  where
    name = symbolName <* delim ':'

objectTypes :: Parser [(String, [(String, GraphQLType)], GraphQLType, Bool)]
objectTypes = sepEndBy1 objectType spaces

objectType :: Parser (String, [(String, GraphQLType)], GraphQLType, Bool)
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

typeNameP :: Parser String
typeNameP = (:) <$> upper <*> many alphaNum

symbolNameP :: Parser String
symbolNameP = (:) <$> lower <*> many alphaNum

enumNameP :: Parser String
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
