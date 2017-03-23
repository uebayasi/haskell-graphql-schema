module Data.GraphQL.XXX.Schema.Token
    ( enumNameP
    , fieldNameP
    , typeNameP
    , graphQlTypeP
    ) where

import           Data.GraphQL.XXX.Schema.AST
import           Text.Parsec
import           Text.Parsec.String

-- Patterns (no "spaces"!)

{- enumNameP
-}
enumNameP :: Parser EnumName
enumNameP = EnumName <$> many1 upper

{- fieldNameP
-}
fieldNameP :: Parser FieldName
fieldNameP = FieldName <$> ((:) <$> lower <*> many alphaNum)

{- typeNameP
-}
typeNameP :: Parser TypeName
typeNameP
    =   graphQlTypeNameQueryP
    <|> graphQlTypeNameMutationP
    <|> graphQlTypeNameP

graphQlTypeNameQueryP :: Parser TypeName
graphQlTypeNameQueryP = pure TypeNameQuery <$> try (string "Query") <?> "Query"

graphQlTypeNameMutationP :: Parser TypeName
graphQlTypeNameMutationP = pure TypeNameMutation <$> try (string "Mutation") <?> "Mutation"

graphQlTypeNameP :: Parser TypeName
graphQlTypeNameP = TypeName <$> ((:) <$> upper <*> many alphaNum)

{- graphQlTypeP
-}
graphQlTypeP :: Parser Type
graphQlTypeP
    =   graphQlBooleanP
    <|> graphQlFloatP
    <|> graphQlIDP
    <|> graphQlIntP
    <|> graphQlListP
    <|> graphQlStringP
    <|> graphQlUserTypeP

graphQlBooleanP :: Parser Type
graphQlBooleanP = pure Boolean <$> try (string "Boolean") <?> "Boolean"

graphQlFloatP :: Parser Type
graphQlFloatP = pure Float <$> try (string "Float") <?> "Float"

graphQlIDP :: Parser Type
graphQlIDP = pure ID <$> try (string "ID") <?> "ID"

graphQlIntP :: Parser Type
graphQlIntP = pure Int <$> try (string "Int") <?> "Int"

graphQlListP :: Parser Type
graphQlListP = List <$> between (char '[') (char ']') graphQlTypeP <?> "List"

graphQlStringP :: Parser Type
graphQlStringP = pure String <$> try (string "String") <?> "String"

graphQlUserTypeP :: Parser Type
graphQlUserTypeP = (\x -> Object x (TypeName "*") False) <$> try typeNameP <?> "User-type"
