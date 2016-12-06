module Main where

import           Data.GraphQL.XXX.Schema
import           System.Environment
import           Text.ParserCombinators.Parsec

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readSchema expr)

readSchema :: String -> String
readSchema input = case parse graphQLStatements "GraphQL Schema" input of
  Left err  -> "Error: " ++ show err
  Right val -> graphQLPretty val
