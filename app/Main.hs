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
    (EnumDefinition name symbols):rest ->
      "Enum! name=" ++ name ++ " symbols=" ++ joinNames ',' (map (\(GraphQLEnumName n) -> n) symbols)
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
