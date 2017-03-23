module Data.GraphQL.XXX.Query.Parser where

import           Data.GraphQL.XXX.Query.AST
import           Data.GraphQL.XXX.Query.Lexer
import           Data.GraphQL.XXX.Query.Token
import           Text.Parsec
import           Text.Parsec.String

graphQLQueries :: Parser [Query]
graphQLQueries = queries query
    where
        query
            =   queryQuery
            <|> mutationQuery
            <|> shorthandQuery

queryQuery :: Parser Query
queryQuery = Query QueryTypeQuery <$> operationDecl "query" <*> args <*> fields

mutationQuery :: Parser Query
mutationQuery = Query QueryTypeMutation <$> operationDecl "mutation" <*> args <*> fields

shorthandQuery :: Parser Query
shorthandQuery = Query QueryTypeQuery (OperationName "") [] <$> fields

args :: Parser [QueryArgument]
args = optionList (parens (sepEndBy1 arg (delim ',')))
    where
        arg :: Parser QueryArgument
        arg = QueryArgument <$> (char '$' *> varName) <* delim ':' <*> graphQlType <*> nonnull

fields :: Parser [Field]
fields = braces (many1 field)

field :: Parser Field
field
    -- XXX fixup leaf nodes (FieldNode -> Field) later
    -- =   Field <$> fieldName <*> pure String <*> pure False
    -- <|> FieldNode <$> fieldName <*> fieldArgs <*> fields <*> pure False
    = FieldNode <$> fieldName <*> optionList fieldArgs <*> optionList fields <*> pure False

fieldArgs :: Parser [FieldArgument]
fieldArgs = parens (sepEndBy1 fieldArg (delim ','))

fieldArg :: Parser FieldArgument
fieldArg
    =   FieldArgument <$> fieldName <* delim ':' <* char '$' <*> varName
    <|> FieldArgumentInput <$> fieldName <*> pure [] -- XXX
