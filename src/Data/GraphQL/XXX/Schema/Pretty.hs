module Data.GraphQL.XXX.Schema.Pretty
    ( graphQLPretty
    ) where

import           Data.GraphQL.XXX.Schema.AST
import           Text.PrettyPrint

class Pretty a where
    pretty :: a -> Doc

graphQLPretty :: [GraphQLStatement] -> String
graphQLPretty statements = render $ vcat (map pretty statements)

{- GraphQLStatement -}

instance Pretty GraphQLStatement where
  pretty (EnumDefinition (GraphQLTypeName t) ns)
    =  text "enum" <+> text t <+> lbrace
    $$ vcat (map oneName ns)
    $$ rbrace
        where
            oneName (GraphQLEnumName e) = nest 2 (text e)

  pretty (InputDefinition (GraphQLTypeName t) fs)
    =  text "input" <+> text t <+> lbrace
    $$ prettyFields fs
    $$ rbrace

  pretty (InterfaceDefinition (GraphQLTypeName t) fs)
    =  text "interface" <+> text t <+> lbrace
    $$ prettyFields fs
    $$ rbrace

  pretty (ObjectDefinition (GraphQLTypeName t) i fs)
    =  text "type" <+> text t <+> implements <+> lbrace
    $$ prettyFields fs
    $$ rbrace
        where
            implements = case i of
                Nothing                    -> empty
                (Just (GraphQLTypeName t)) -> text "implements" <+> text t

  pretty (ScalarDefinition (GraphQLTypeName t))
    =   text "scalar"
    <+> text t

  pretty (UnionDefinition (GraphQLTypeName t) (GraphQLTypeName n:ns))
    = text "union" <+> text t <+> char '=' <+> text n <+> hcat (restNames ns)
        where
            restNames = map ((char '|' <+>) . oneName)
            oneName (GraphQLTypeName t) = text t

{- Other types -}

prettyFields :: GraphQLFields -> Doc
prettyFields fs = vcat (map pretty fs)

instance Pretty GraphQLField where
  pretty (GraphQLField (GraphQLFieldName f) as t b) = nest 2 field
    where
        field = text f <> prettyArguments as <> colon <+> pretty t <> exclamation
        exclamation = if b then char '!' else empty

prettyArguments :: GraphQLArguments -> Doc
prettyArguments args = case args of
    [] -> empty
    (a:as) -> lparen <> pretty a <> vcat restArgs <> rparen
        where
            restArgs = map ((comma <+>) . pretty) as

instance Pretty GraphQLArgument where
    pretty (GraphQLArgument (GraphQLFieldName n) t) = text n <> colon <+> pretty t

instance Pretty GraphQLType where
    pretty t = case t of
        GraphQLBoolean                       -> text "Boolean"
        GraphQLFloat                         -> text "Float"
        GraphQLList t'                       -> lbrack <> pretty t' <> rbrack
        GraphQLID                            -> text "ID"
        GraphQLInt                           -> text "Int"
        GraphQLString                        -> text "String"
        GraphQLUserType (GraphQLTypeName t') -> text t'
