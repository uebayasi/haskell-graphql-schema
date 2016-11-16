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
    (TypeDefinition name _):rest ->
      "Type! name=" ++ name

data GraphQLStatement
  = EnumDefinition String [String]
  | ScalarDefinition String
  | TypeDefinition String [(String, GraphQLType, Bool)]

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
  statements <- many $ enumDefinition <|> scalarDefinition <|> typeDefinition
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
  args <- option [] $ parens typeArgs
  spaces
  types <- braces typeTypes
  spaces
  return $ TypeDefinition name types

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

typeTypes :: Parser [(String, GraphQLType, Bool)]
typeTypes = do
  spaces
  types <- sepEndBy1 typeType spaces
  spaces
  return $ types

typeType :: Parser (String, GraphQLType, Bool)
typeType = do
  spaces
  name <- memberName
  spaces
  char ':'
  spaces
  gtype <- graphQlType
  spaces
  nonnull <- option False $ (do { char '!'; return True })
  spaces
  return $ (name, gtype, nonnull)

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
