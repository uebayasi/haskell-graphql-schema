module Main where

import           Data.GraphQL.Schema
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
    (EnumDefinition (GraphQLTypeName name) symbols):rest ->
      "Enum! name=" ++ name ++ " symbols=" ++ joinNames ',' (map (\(GraphQLEnumName n) -> n) symbols)
    InputDefinition (GraphQLTypeName name) _:rest ->
      "Input! name=" ++ name
    InterfaceDefinition (GraphQLTypeName name) _:rest ->
      "Interface! name=" ++ name
    ScalarDefinition (GraphQLTypeName name):rest ->
      "Scalar! name=" ++ name
    ObjectDefinition (GraphQLTypeName name) Nothing _:rest ->
      "Type! name=" ++ name
    ObjectDefinition (GraphQLTypeName name) (Just (GraphQLTypeName ifname)) _:rest ->
      "Type! name=" ++ name ++ " ifname=" ++ ifname
    UnionDefinition (GraphQLTypeName name) utypes:rest ->
      "Union! name=" ++ name ++ " utypes=" ++ joinNames '|' (map (\(GraphQLTypeName n) -> n) utypes)

joinNames :: Char -> [String] -> String
joinNames sep names =
  case names of
    []         -> ""
    first:[]   -> first
    first:rest -> first ++ concatMap ((:) sep) rest
