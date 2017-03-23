module Data.GraphQL.XXX.Schema.Parser
    ( graphQLStatements
    ) where

import           Data.GraphQL.XXX.Schema.AST
import           Data.GraphQL.XXX.Schema.Lexer
import           Data.GraphQL.XXX.Schema.Token
import           Text.Parsec
import           Text.Parsec.String

graphQLStatements :: Parser [Statement]
graphQLStatements = statements statement
    where
        statement
          =   enumDefinition
          <|> inputDefinition
          <|> interfaceDefinition
          <|> objectDefinition
          <|> scalarDefinition
          <|> unionDefinition

-- Enum

enumDefinition :: Parser Statement
enumDefinition = EnumDefinition <$> typeDecl "enum" <*> symbols
    where
        symbols = braces enumSymbols

        enumSymbols :: Parser EnumNames
        enumSymbols = sepEndBy1 enumName spaces

-- Input

inputDefinition :: Parser Statement
inputDefinition = InputDefinition <$> typeDecl "input" <*> ifields
    where
        ifields = braces inputFields

inputFields :: Parser [InputField]
inputFields = sepEndBy1 inputField spaces
    where
        inputField :: Parser InputField
        inputField = InputField <$> fieldName <*> graphQlTypeName <*> nonnull
            where
                nonnull = optionBool $ delim '!'

-- Interface

interfaceDefinition :: Parser Statement
interfaceDefinition = (\t fs -> InterfaceDefinition t [] fs) <$> typeDecl "interface" <*> fields
    where
        fields = braces objectFields

-- Object

objectDefinition :: Parser Statement
objectDefinition = ObjectDefinition <$> typeDecl "type" <*> ifname <*> fields
    where
        ifname = optionMaybe $ typeDecl "implements"
        fields = braces objectFields

objectFields :: Parser [Field]
objectFields = sepEndBy1 objectField spaces
    where
        objectField :: Parser Field
        objectField = (\a b c d -> Field a b c d Nothing) <$> fieldName <*> args <* delim ':' <*> graphQlTypeName <*> nonnull
            where
                args = optionList $ parens objectArgs
                nonnull = optionBool $ delim '!'

                objectArgs :: Parser [Argument]
                objectArgs = sepEndBy1 objectArg (delim ',')
                    where
                        objectArg :: Parser Argument
                        objectArg = Argument <$> fieldName <* delim ':' <*> graphQlTypeName <*> nonnull

-- Scalar

scalarDefinition :: Parser Statement
scalarDefinition = ScalarDefinition <$> typeDecl "scalar"

-- Union

unionDefinition :: Parser Statement
unionDefinition = UnionDefinition <$> typeDecl "union" <* delim '=' <*> types
    where
        types = sepBy1 typeName (delim '|')
