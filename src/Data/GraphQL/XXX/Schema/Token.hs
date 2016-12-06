module Data.GraphQL.XXX.Schema.Token
  ( enumNameP
  , fieldNameP
  , typeNameP
  , graphQlTypeP
  , graphQlBooleanP
  , graphQlFloatP
  , graphQlIDP
  , graphQlIntP
  , graphQlListP
  , graphQlStringP
  , graphQlUserTypeP
  ) where

import           Data.GraphQL.XXX.Schema.AST
import           Text.Parsec
import           Text.Parsec.String

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
graphQlBooleanP = pure GraphQLBoolean <$> try (string "Boolean") <?> "Boolean"

graphQlFloatP :: Parser GraphQLType
graphQlFloatP = pure GraphQLFloat <$> try (string "Float") <?> "Float"

graphQlIDP :: Parser GraphQLType
graphQlIDP = pure GraphQLID <$> try (string "ID") <?> "ID"

graphQlIntP :: Parser GraphQLType
graphQlIntP = pure GraphQLInt <$> try (string "Int") <?> "Int"

graphQlListP :: Parser GraphQLType
graphQlListP = GraphQLList <$> between (char '[') (char ']') graphQlTypeP <?> "List"

graphQlStringP :: Parser GraphQLType
graphQlStringP = pure GraphQLString <$> try (string "String") <?> "String"

graphQlUserTypeP :: Parser GraphQLType
graphQlUserTypeP = GraphQLUserType <$> try typeNameP <?> "User-type"
