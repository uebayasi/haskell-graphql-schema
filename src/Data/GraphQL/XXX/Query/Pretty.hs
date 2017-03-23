module Data.GraphQL.XXX.Query.Pretty
    ( queryPretty
    , queryString
    ) where

import           Data.GraphQL.XXX.Query.AST
import           Text.PrettyPrint

class Pretty a where
    pretty :: a -> Doc
    string :: a -> Doc

queryPretty :: [Query] -> String
queryPretty queries = render $ vcat (map pretty queries)

queryString :: [Query] -> String
queryString queries =
    case queries of
        [] ->
            ""
        (q:qs) ->
            render $
                string q <> hcat restArgs
            where
                restArgs = map (\q -> space <> string q) qs

{------------------------------------------------------------------------------}

instance Pretty Query where
    pretty (Query qt qop qargs qfields)
        =   text (qdecl qt)
        <+> text (getName qop)
        <>  prettyArgs qargs
        <+> prettyFields qfields

    string (Query qt qop qargs qfields)
        =   text (qdecl qt)
        <+> text (getName qop)
        <>  stringArgs qargs
        <+> stringFields qfields

instance Pretty QueryArgument where
    pretty (QueryArgument an at ann)
        =   text "(qarg)"

    string (QueryArgument an at ann)
        =   char '$' <> text (getName an) <> char ':' <+> string at <> stringNonnull ann

instance Pretty Field where
    pretty (Field fn ft fnn)
        -- `ft' and `fnn' are not part of original query
        =   text "(field)"
    pretty (FieldNode fn fargs fields fnn)
        -- `fnn' is not part of original query
        =   text (getName fn)
        <>  prettyArgs fargs
        <+> prettyFields fields

    string (Field fn ft fnn)
        -- `ft' and `fnn' are not part of original query
        =   text (getName fn)
    string (FieldNode fn fargs fields fnn)
        -- `fnn' is not part of original query
        =   text (getName fn) <> stringArgs fargs <+> stringFields fields

instance Pretty FieldArgument where
    pretty (FieldArgument fn fvn)
        =   text (getName fn) <> char ':' <+> char '$' <> text (getName fvn)
    pretty (FieldArgumentInput fn fargs)
        =   text "(fieldarginput)"

    string (FieldArgument fn fvn)
        =   text (getName fn) <> char ':' <+> char '$' <> text (getName fvn)
    string (FieldArgumentInput fn fargs)
        =   text "(fieldarginput)"

instance Pretty Type where
    pretty t = case t of
        Boolean    -> text "Boolean"
        Float      -> text "Float"
        ID         -> text "ID"
        Int        -> text "Int"
        String     -> text "String"
        UserType x -> text (getName x)
        List x     -> brackets (string x)

    string = pretty

{------------------------------------------------------------------------------}

qdecl :: QueryType -> String
qdecl qt = case qt of
    QueryTypeQuery         -> "query"
    QueryTypeMutation      -> "mutation"
    QueryTypeIntrospection -> "introspection"

prettyArgs :: (Pretty a) => [a] -> Doc
prettyArgs = stringArgs

stringArgs :: (Pretty a) => [a] -> Doc
stringArgs qargs =
    case qargs of
        [] ->
            empty
        (a:as) ->
            parens (string a <> hcat restArgs)
            where
                restArgs = map ((comma <+>) . string) as

prettyFields :: (Pretty a) => [a] -> Doc
prettyFields fs
    =   lbrace
    $+$ nest 2 (vcat (map pretty fs))
    $+$ rbrace

stringFields :: (Pretty a) => [a] -> Doc
stringFields fields =
    case fields of
        [] ->
            empty
        (f:fs) ->
            braces (space <> string f <> hcat restArgs <> space)
            where
                restArgs = map (\f -> space <> string f) fs

stringNonnull :: Bool -> Doc
stringNonnull nn = if nn then char '!' else empty
