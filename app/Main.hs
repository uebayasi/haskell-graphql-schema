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
      "Enum! name=" ++ name
    (ScalarDefinition name):rest ->
      "Scalar! name=" ++ name
    (TypeDefinition name ifname _):rest ->
      "Type! name=" ++ name ++ " ifname=" ++ ifname
    (UnionDefinition name _):rest ->
      "Union! name=" ++ name

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
graphQLStatements = do
  spaces
  statements <- many $ enumDefinition <|> interfaceDefinition <|> scalarDefinition <|> typeDefinition <|> unionDefinition
  spaces
  return $ statements

-- Enum

enumDefinition :: Parser GraphQLStatement
enumDefinition = do
  spaces
  string "enum"
  spaces
  name <- enumName
  spaces
  symbols <- braces enumSymbols
  spaces
  return $ EnumDefinition name symbols

enumName :: Parser String
enumName = typeName

enumSymbols :: Parser [String]
enumSymbols = do
  spaces
  symbols <- sepEndBy1 (many1 upper) spaces
  spaces
  return $ symbols

-- Interface

interfaceDefinition :: Parser GraphQLStatement
interfaceDefinition = do
  spaces
  string "interface"
  spaces
  name <- typeName
  spaces
  args <- option [] $ parens typeArgs
  spaces
  types <- braces typeTypes
  spaces
  return $ InterfaceDefinition name types

-- Scalar

scalarDefinition :: Parser GraphQLStatement
scalarDefinition = do
  spaces
  string "scalar"
  spaces
  name <- scalarName
  return $ ScalarDefinition name

scalarName :: Parser String
scalarName = typeName

-- Type

typeDefinition :: Parser GraphQLStatement
typeDefinition = do
  spaces
  string "type"
  spaces
  name <- typeName
  spaces
  ifname <- option [] $ do
    string "implements"
    spaces
    ifname <- typeName
    spaces
    return ifname
  spaces
  types <- braces typeTypes
  spaces
  return $ TypeDefinition name ifname types

typeArgs :: Parser [(String, GraphQLType)]
typeArgs = do
  spaces
  args <- sepEndBy1 typeArg (spaces >> (char ',') >> spaces)
  spaces
  return $ args

typeArg :: Parser (String, GraphQLType)
typeArg = do
  spaces
  name <- memberName
  spaces
  char ':'
  spaces
  gtype <- graphQlType
  spaces
  return $ (name, gtype)

typeTypes :: Parser [(String, [(String, GraphQLType)], GraphQLType, Bool)]
typeTypes = do
  spaces
  types <- sepEndBy1 typeType spaces
  spaces
  return $ types

typeType :: Parser (String, [(String, GraphQLType)], GraphQLType, Bool)
typeType = do
  spaces
  name <- memberName
  spaces
  args <- option [] $ parens typeArgs
  spaces
  char ':'
  spaces
  gtype <- graphQlType
  spaces
  nonnull <- option False $ do
    char '!'
    return True
  spaces
  return $ (name, args, gtype, nonnull)

-- Union

unionDefinition :: Parser GraphQLStatement
unionDefinition = do
  spaces
  string "union"
  spaces
  name <- typeName
  spaces
  char '='
  spaces
  utypes <- unionTypes
  spaces
  return $ UnionDefinition name utypes

unionTypes :: Parser [String]
unionTypes = do
  spaces
  utypes <- sepBy1 unionType (char '|')
  spaces
  return utypes

unionType :: Parser String
unionType = do
  spaces
  utype <- typeName
  spaces
  return utype

-- Common

typeName :: Parser String
typeName = do
  first <- upper
  rest <- many alphaNum
  return $ first:rest

memberName :: Parser String
memberName = do
  first <- lower
  rest <- many alphaNum
  return $ first:rest

graphQlType :: Parser GraphQLType
graphQlType = graphQlTypeBoolean <|> graphQlTypeFloat <|> graphQlTypeInt <|> graphQlTypeList <|> graphQlTypeString <|> graphQlTypeUser

graphQlTypeBoolean :: Parser GraphQLType
graphQlTypeBoolean = do
  string "Boolean"
  return $ GraphQLTypeBoolean

graphQlTypeFloat :: Parser GraphQLType
graphQlTypeFloat = do
  string "Float"
  return $ GraphQLTypeFloat

graphQlTypeInt :: Parser GraphQLType
graphQlTypeInt = do
  string "Int"
  return $ GraphQLTypeInt

graphQlTypeList :: Parser GraphQLType
graphQlTypeList = do
  elem <- brackets graphQlType
  return $ GraphQLTypeList elem

graphQlTypeString :: Parser GraphQLType
graphQlTypeString = do
  string "String"
  return $ GraphQLTypeString

graphQlTypeUser :: Parser GraphQLType
graphQlTypeUser = do
  name <- typeName
  return $ GraphQLTypeUser name

braces =
  between (char '{') (char '}')

brackets =
  between (char '[') (char ']')

parens =
  between (char '(') (char ')')
