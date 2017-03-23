module Data.GraphQL.XXX.Schema.Pretty
    ( graphQLPretty
    ) where

import           Data.GraphQL.XXX.Schema.AST
import           Text.PrettyPrint

class Pretty a where
    pretty :: a -> Doc

graphQLPretty :: [Statement] -> String
graphQLPretty statements = render $ vcat (map pretty statements)

{- Statement -}

instance Pretty Statement where
    pretty (EnumDefinition (TypeName t) ns)
        =  text "enum" <+> text t <+> lbrace
        $$ vcat (map oneName ns)
        $$ rbrace
            where
                oneName (EnumName e) = nest 2 (text e)

    pretty (InputDefinition (TypeName t) ifs)
        =  text "input" <+> text t <+> lbrace
        $$ prettyInputFields ifs
        $$ rbrace

    pretty (InterfaceDefinition (TypeName t) _ fs)
        =  text "interface" <+> text t <+> lbrace
        $$ prettyFields fs
        $$ rbrace

    pretty (ObjectDefinition (TypeName t) i fs)
        =  text "type" <+> text t <+> intf <+> lbrace
        $$ prettyFields fs
        $$ rbrace
            where
                intf = case i of
                    Nothing             -> empty
                    (Just (TypeName t)) -> text "implements" <+> text t

    pretty (ScalarDefinition (TypeName t))
        =  text "scalar" <+> text t

    pretty (UnionDefinition (TypeName t) (TypeName n:ns))
        =  text "union" <+> text t <+> char '=' <+> text n <+> hcat (restNames ns)
            where
                restNames = map ((char '|' <+>) . oneName)
                oneName (TypeName t) = text t

{- Other types -}

prettyFields :: Fields -> Doc
prettyFields =
    vcat . map pretty

instance Pretty Field where
    pretty (Field (FieldName f) as t nn _) =
        nest 2 field
        where
            field = text f <> prettyArguments as <> colon <+> pretty t <> exclamation
            exclamation = if nn then char '!' else empty

prettyInputFields :: InputFields -> Doc
prettyInputFields =
    vcat . map pretty

instance Pretty InputField where
    pretty (InputField fn ft fnn) =
        pretty (Field fn [] ft fnn Nothing)

prettyArguments :: Arguments -> Doc
prettyArguments args = case args of
    [] -> empty
    (a:as) -> lparen <> pretty a <> vcat restArgs <> rparen
        where
            restArgs = map ((comma <+>) . pretty) as

instance Pretty Argument where
    pretty (Argument (FieldName n) t nn) =
        text n <> colon <+> pretty t <> exclamation
        where
            exclamation = if nn then char '!' else empty

instance Pretty Type where
    pretty t = case t of
        Boolean                  -> text "Boolean"
        Float                    -> text "Float"
        List t'                  -> lbrack <> pretty t' <> rbrack
        ID                       -> text "ID"
        Int                      -> text "Int"
        String                   -> text "String"
        Object (TypeName t') _ _ -> text t'
        Enum (TypeName t')       -> text t'
        Scalar (TypeName t') _   -> text t'
