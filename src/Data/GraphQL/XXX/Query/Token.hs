module Data.GraphQL.XXX.Query.Token
    ( enumNameP
    , fieldNameP
    , varNameP
    , typeNameP
    , operationNameP
    , typeP
    ) where

import qualified Data.GraphQL.XXX.Query.AST as Query
import           Text.Parsec
import           Text.Parsec.String

-- Patterns (no "spaces"!)

enumNameP :: Parser Query.EnumName
enumNameP = Query.EnumName <$> try (many1 upper)

fieldNameP :: Parser Query.FieldName
fieldNameP = Query.FieldName <$> try ((:) <$> lower <*> many alphaNum)

varNameP :: Parser Query.VarName
varNameP = Query.VarName <$> try ((:) <$> lower <*> many alphaNum)

typeNameP :: Parser Query.TypeName
typeNameP = Query.TypeName <$> try ((:) <$> upper <*> many alphaNum)

operationNameP :: Parser Query.OperationName
operationNameP = Query.OperationName <$> try ((:) <$> letter <*> many alphaNum)

typeP :: Parser Query.Type
typeP
    =   typeBooleanP
    <|> typeFloatP
    <|> typeIDP
    <|> typeIntP
    <|> typeListP
    <|> typeStringP
    <|> typeUserTypeP

typeBooleanP :: Parser Query.Type
typeBooleanP = pure Query.Boolean <$> try (string "Boolean") <?> "Boolean"

typeFloatP :: Parser Query.Type
typeFloatP = pure Query.Float <$> try (string "Float") <?> "Float"

typeIDP :: Parser Query.Type
typeIDP = pure Query.ID <$> try (string "ID") <?> "ID"

typeIntP :: Parser Query.Type
typeIntP = pure Query.Int <$> try (string "Int") <?> "Int"

typeListP :: Parser Query.Type
typeListP = Query.List <$> between (char '[') (char ']') typeP <?> "List"

typeStringP :: Parser Query.Type
typeStringP = pure Query.String <$> try (string "String") <?> "String"

typeUserTypeP :: Parser Query.Type
typeUserTypeP = Query.UserType <$> try typeNameP <?> "User-type"
