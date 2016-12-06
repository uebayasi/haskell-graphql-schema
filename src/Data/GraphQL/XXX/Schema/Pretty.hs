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
    (InputDefinition t fs)     -> prettyInput t fs
    (InterfaceDefinition t fs) -> prettyInterface t fs
    (ObjectDefinition t i fs)  -> prettyObject t i fs
    (ScalarDefinition t)       -> prettyScalar t
    (UnionDefinition t ns)     -> prettyUnion t ns

--

prettyEnum :: GraphQLTypeName -> GraphQLEnumNames -> Doc
prettyEnum (GraphQLTypeName t) ns
    =  text "enum" <+> text t <+> lbrace
    $$ vcat (map (\(GraphQLEnumName e) -> nest 2 (text e)) ns)
    $$ rbrace

prettyInput :: GraphQLTypeName -> GraphQLFields -> Doc
prettyInput (GraphQLTypeName t) fs
    =  text "input" <+> text t <+> lbrace
    $$ prettyFields fs
    $$ rbrace

prettyInterface :: GraphQLTypeName -> GraphQLFields -> Doc
prettyInterface (GraphQLTypeName t) fs
    =  text "interface" <+> text t <+> lbrace
    $$ prettyFields fs
    $$ rbrace

prettyObject :: GraphQLTypeName -> Maybe GraphQLTypeName -> GraphQLFields -> Doc
prettyObject (GraphQLTypeName t) i fs
    =  text "type" <+> text t <+> implements <+> lbrace
    $$ prettyFields fs
    $$ rbrace
        where
            implements = case i of
                Nothing                    -> empty
                (Just (GraphQLTypeName t)) -> text "implements" <+> text t

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

--

prettyFields :: GraphQLFields -> Doc
prettyFields fs = vcat (map prettyField fs)

prettyField :: GraphQLField -> Doc
prettyField (GraphQLField (GraphQLFieldName f) as t b) =
    nest 2 (text f <> prettyArguments as <> colon <+> prettyType t <> exclamation)
        where
            exclamation = if b then char '!' else empty

prettyArguments :: GraphQLArguments -> Doc
prettyArguments args = case args of
    [] -> empty
    (a:as) -> lparen <> prettyArgument a <> vcat (map ((comma <+>) . prettyArgument) as) <> rparen

prettyArgument :: GraphQLArgument -> Doc
prettyArgument (GraphQLArgument (GraphQLFieldName n) t) = text n <> colon <+> prettyType t

prettyType :: GraphQLType -> Doc
prettyType t = case t of
    GraphQLBoolean                       -> text "Boolean"
    GraphQLFloat                         -> text "Boolean"
    GraphQLList t'                       -> lbrack <> prettyType t' <> rbrack
    GraphQLID                            -> text "ID"
    GraphQLInt                           -> text "Int"
    GraphQLString                        -> text "String"
    GraphQLUserType (GraphQLTypeName t') -> text t'
