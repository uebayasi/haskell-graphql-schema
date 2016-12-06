module Data.GraphQL.XXX.Schema.Pretty
    ( graphQLPretty
    ) where

import           Data.GraphQL.XXX.Schema.AST
import           Text.PrettyPrint

graphQLPretty :: [GraphQLStatement] -> String
graphQLPretty statements = render $ prettyStatements statements
    where
        prettyStatements :: [GraphQLStatement] -> Doc
        prettyStatements statements = case statements of
            []           -> empty
            (first:rest) -> prettyStatement first $$ prettyStatements rest

prettyStatement :: GraphQLStatement -> Doc
prettyStatement statement = case statement of
    (EnumDefinition t ns)      -> text "enum!"
    (InputDefinition t fs)     -> text "input!"
    (InterfaceDefinition t fs) -> text "interface!"
    (ObjectDefinition t i fs)  -> text "object!"
    (ScalarDefinition t)       -> text "scalar!"
    (UnionDefinition t ns)     -> prettyUnion t ns

prettyUnion :: GraphQLTypeName -> GraphQLTypeNames -> Doc
prettyUnion (GraphQLTypeName t) (GraphQLTypeName n:ns)
    =   text "union"
    <+> text t
    <+> char '='
    <+> text n
    <+> restNames ns
        where
            restNames names = case names of
                []              -> empty
                (GraphQLTypeName n:ns) ->
                    char '|' <+> text n <+> restNames ns
