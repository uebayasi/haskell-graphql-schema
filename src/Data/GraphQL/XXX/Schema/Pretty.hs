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
    (EnumDefinition t ns)      -> prettyEnum t ns
    (InputDefinition t fs)     -> text "input!"
    (InterfaceDefinition t fs) -> text "interface!"
    (ObjectDefinition t i fs)  -> text "object!"
    (ScalarDefinition t)       -> prettyScalar t
    (UnionDefinition t ns)     -> prettyUnion t ns

prettyEnum :: GraphQLTypeName -> GraphQLEnumNames -> Doc
prettyEnum (GraphQLTypeName t) ns
    =  text "enum" <+> text t <+> text "{"
    $$ vcat (map (\(GraphQLEnumName e) -> nest 2 (text e)) ns)
    $$ text "}"

prettyScalar :: GraphQLTypeName -> Doc
prettyScalar (GraphQLTypeName t)
    =   text "scalar"
    <+> text t

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
