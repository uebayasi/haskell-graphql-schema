module Main where

import Lib
import System.Environment
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readSchema expr)

readSchema :: String -> String
readSchema input = case (parse (graphQLStatements) "GraphQL Schema" input) of
  Left err -> "Error: " ++ show err
  Right val -> case val of
    (EnumDefinition name symbols):rest ->
      "Enum! name=" ++ name ++ " symbols=" ++ (joinNames ',' symbols)
    (InterfaceDefinition name _):rest ->
      "Interface! name=" ++ name
    (ScalarDefinition name):rest ->
      "Scalar! name=" ++ name
    (TypeDefinition name ifname _):rest ->
      "Type! name=" ++ name ++ " ifname=" ++ ifname
    (UnionDefinition name utypes):rest ->
      "Union! name=" ++ name ++ " utypes=" ++ (joinNames '|' utypes)

joinNames :: Char -> [String] -> String
joinNames sep names =
  case names of
    [] -> ""
    first:[] -> first
    first:rest -> first ++ (concat $ map ((:) sep) rest)

data GraphQLStatement
  = EnumDefinition String [String]
  | InterfaceDefinition String [(String, [(String, GraphQLType)], GraphQLType, Bool)]
  | ScalarDefinition String
  | TypeDefinition String String [(String, [(String, GraphQLType)], GraphQLType, Bool)]
  | UnionDefinition String [String]

data GraphQLType
  = GraphQLTypeBoolean
  | GraphQLTypeFloat
  | GraphQLTypeList GraphQLType
  | GraphQLTypeInt
  | GraphQLTypeString
  | GraphQLTypeUser String

graphQLStatements :: Parser [GraphQLStatement]
graphQLStatements = spaces *> statements <* spaces
  where
    statements = many $ enumDefinition <|> interfaceDefinition <|> scalarDefinition <|> typeDefinition <|> unionDefinition

-- Enum

enumDefinition :: Parser GraphQLStatement
enumDefinition = EnumDefinition <$> name <*> symbols
  where
    name = (keyword "enum") *> typeName
    symbols = braces enumSymbols

enumSymbols :: Parser [String]
enumSymbols = sepEndBy1 (many1 upper) spaces

-- Interface

interfaceDefinition :: Parser GraphQLStatement
interfaceDefinition = InterfaceDefinition <$> name <*> types
  where
    name = (keyword "interface") *> typeName
    args = option [] $ parens typeArgs
    types = braces typeTypes

-- Scalar

scalarDefinition :: Parser GraphQLStatement
scalarDefinition = ScalarDefinition <$> name
  where
    name = (keyword "scalar") *> typeName

-- Type

typeDefinition :: Parser GraphQLStatement
typeDefinition = TypeDefinition <$> name <*> ifname <*> types
  where
    name = (keyword "type") *> typeName
    ifname = option [] $ (keyword "implements") *> typeName
    types = braces typeTypes

typeArgs :: Parser [(String, GraphQLType)]
typeArgs = sepEndBy1 typeArg (delim ',')

typeArg :: Parser (String, GraphQLType)
typeArg = (,) <$> name <*> graphQlTypeName
  where
    name = symbolName <* (delim ':')

typeTypes :: Parser [(String, [(String, GraphQLType)], GraphQLType, Bool)]
typeTypes = sepEndBy1 typeType spaces

typeType :: Parser (String, [(String, GraphQLType)], GraphQLType, Bool)
typeType = (,,,) <$> name <*> args <*> ttype <*> nonnull
  where
    name = symbolName
    args = option [] $ parens typeArgs
    ttype = (delim ':') *> graphQlTypeName
    nonnull = option False $ (delim '!') *> pure True

-- Union

unionDefinition :: Parser GraphQLStatement
unionDefinition = UnionDefinition <$> name <*> types
  where
    name = (keyword "union") *> typeName <* (delim '=')
    types = sepBy1 typeName (delim '|')

-- Common

typeName = spaces *> typeNameP <* spaces
symbolName = spaces *> symbolNameP <* spaces
graphQlTypeName = spaces *> graphQlType <* spaces

braces :: Parser a -> Parser a
braces = between (delim '{') (delim '}')

brackets :: Parser a -> Parser a
brackets = between (delim '[') (delim ']')

parens :: Parser a -> Parser a
parens = between (delim '(') (delim ')')

keyword :: String -> Parser ()
keyword s = spaces *> (string s) *> spaces

delim :: Char -> Parser ()
delim c = spaces *> (char c) *> spaces

-- Patterns (no "spaces"!)

typeNameP :: Parser String
typeNameP = (:) <$> upper <*> (many alphaNum)

symbolNameP :: Parser String
symbolNameP = (:) <$> lower <*> (many alphaNum)

graphQlType :: Parser GraphQLType
graphQlType = graphQlTypeBoolean <|> graphQlTypeFloat <|> graphQlTypeInt <|> graphQlTypeList <|> graphQlTypeString <|> graphQlTypeUser

graphQlTypeBoolean :: Parser GraphQLType
graphQlTypeBoolean = pure GraphQLTypeBoolean <$> (string "Boolean")

graphQlTypeFloat :: Parser GraphQLType
graphQlTypeFloat = pure GraphQLTypeFloat <$> (string "Float")

graphQlTypeInt :: Parser GraphQLType
graphQlTypeInt = pure GraphQLTypeInt <$> (string "Int")

graphQlTypeList :: Parser GraphQLType
graphQlTypeList = GraphQLTypeList <$> brackets graphQlType

graphQlTypeString :: Parser GraphQLType
graphQlTypeString = pure GraphQLTypeString <$> (string "String")

graphQlTypeUser :: Parser GraphQLType
graphQlTypeUser = GraphQLTypeUser <$> typeNameP
