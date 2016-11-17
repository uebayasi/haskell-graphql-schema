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
graphQLStatements = do
  spaces
  statements <- many $ enumDefinition <|> interfaceDefinition <|> scalarDefinition <|> typeDefinition <|> unionDefinition
  spaces
  return $ statements

-- Enum

enumDefinition :: Parser GraphQLStatement
enumDefinition = do
  keyword "enum"
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
  keyword "interface"
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
  keyword "scalar"
  name <- scalarName
  spaces
  return $ ScalarDefinition name

scalarName :: Parser String
scalarName = typeName

-- Type

typeDefinition :: Parser GraphQLStatement
typeDefinition = do
  keyword "type"
  name <- typeName
  spaces
  ifname <- option [] $ do
    keyword "implements"
    ifname <- typeName
    spaces
    return ifname
  spaces
  types <- braces typeTypes
  spaces
  return $ TypeDefinition name ifname types

typeArgs :: Parser [(String, GraphQLType)]
typeArgs = do
  let
    args' = sepEndBy1 typeArg (spaces >> (char ',') >> spaces)
  spaces
  args <- args'
  spaces
  return $ args

typeArg :: Parser (String, GraphQLType)
typeArg = (,) <$> name <*> gtype
  where
    name = spaces *> memberName <* spaces <* (char ':') <* spaces
    gtype = spaces *> graphQlType <* spaces

typeTypes :: Parser [(String, [(String, GraphQLType)], GraphQLType, Bool)]
typeTypes = spaces *> (sepEndBy1 typeType spaces) <* spaces

typeType :: Parser (String, [(String, GraphQLType)], GraphQLType, Bool)
typeType = (,,,) <$> name <*> args <*> gtype <*> nonnull
  where
    name = spaces *> memberName <* spaces
    args = spaces *> (option [] $ parens typeArgs) <* spaces <* (char ':') <* spaces
    gtype = spaces *> graphQlType <* spaces
    nonnull =  spaces *> (option False $ (char '!') *> pure True) <* spaces

-- Union

unionDefinition :: Parser GraphQLStatement
unionDefinition = UnionDefinition <$> name <*> utypes
  where
    name = keyword "union" *> typeName <* spaces <* (char '=') <* spaces
    utypes = unionTypes <* spaces

unionTypes :: Parser [String]
unionTypes = spaces *> (sepBy1 unionType (char '|')) <* spaces

unionType :: Parser String
unionType = spaces *> typeName <* spaces

-- Common

typeName :: Parser String
typeName = (:) <$> upper <*> (many alphaNum)

memberName :: Parser String
memberName = (:) <$> lower <*> (many alphaNum)

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
graphQlTypeUser = GraphQLTypeUser <$> typeName

braces =
  between (char '{') (char '}')

brackets =
  between (char '[') (char ']')

parens =
  between (char '(') (char ')')

keyword :: String -> Parser ()
keyword s = spaces *> (string s) *> spaces
