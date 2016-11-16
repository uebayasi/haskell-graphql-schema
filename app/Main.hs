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
    EnumDefinition name symbols ->
      "Enum! name=" ++ name

data GraphQLStatement
  = EnumDefinition String [String]

graphQLStatements :: Parser GraphQLStatement
graphQLStatements =
  enumDefinition

enumDefinition :: Parser GraphQLStatement
enumDefinition = do
  spaces
  string "enum"
  spaces
  name <- enumName
  spaces
  symbols <- braces enumSymbols
  return $ EnumDefinition name symbols

enumName :: Parser String
enumName = do
  first <- upper
  rest <- many alphaNum
  return $ first:rest

enumSymbols :: Parser [String]
enumSymbols = do
  spaces
  symbols <- sepEndBy1 (many1 upper) spaces
  spaces
  return $ symbols

braces =
  between (char '{') (char '}')
