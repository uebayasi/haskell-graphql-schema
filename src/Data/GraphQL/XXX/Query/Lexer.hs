module Data.GraphQL.XXX.Query.Lexer
    ( enumName
    , fieldName
    , varName
    , typeName
    , graphQlType
    , queries
    , operationDecl
    , braces
    , parens
    , delim
    , nonnull
    , optionList
    , optionBool
    , dump
    ) where

import qualified Data.GraphQL.XXX.Query.AST   as Query
import           Data.GraphQL.XXX.Query.Token
import           Debug.Trace                  (trace)
import           Text.Parsec
import           Text.Parsec.String

{-

Fields

    {
      hero {
        name
      }
    }

    {
      hero {
        name
        # Queries can have comments!
        friends {
          name
        }
      }
    }

Arguments

    {
      human(id: "1000") {
        name
        height
      }
    }

    {
      human(id: "1000") {
        name
        height(unit: FOOT)
      }
    }

Aliases

    {
      empireHero: hero(episode: EMPIRE) {
        name
      }
      jediHero: hero(episode: JEDI) {
        name
      }
    }

Fragments

    {
      leftComparison: hero(episode: EMPIRE) {
        ...comparisonFields
      }
      rightComparison: hero(episode: JEDI) {
        ...comparisonFields
      }
    }

    fragment comparisonFields on Character {
      name
      appearsIn
      friends {
        name
      }
    }

Variables

    query HeroNameAndFriends($episode: Episode) {
      hero(episode: $episode) {
        name
        friends {
          name
        }
      }
    }

Directives

    query Hero($episode: Episode, $withFriends: Boolean!) {
      hero(episode: $episode) {
        name
        friends @include(if: $withFriends) {
          name
        }
      }
    }

Mutations

    mutation CreateReviewForEpisode($ep: Episode!, $review: ReviewInput!) {
      createReview(episode: $ep, review: $review) {
        stars
        commentary
      }
    }

Inline Fragments

    query HeroForEpisode($ep: Episode!) {
      hero(episode: $ep) {
        name
        ... on Droid {
          primaryFunction
        }
        ... on Human {
          height
        }
      }
    }

Meta Fields

    {
      search(text: "an") {
        __typename
        ... on Human {
          name
        }
        ... on Droid {
          name
        }
        ... on Starship {
          name
        }
      }
    }

-}

-- Common

enumName :: Parser Query.EnumName
enumName = spaces *> enumNameP <* spaces

fieldName :: Parser Query.FieldName
fieldName = spaces *> fieldNameP <* spaces

varName :: Parser Query.VarName
varName = spaces *> varNameP <* spaces

typeName :: Parser Query.TypeName
typeName = spaces *> typeNameP <* spaces

operationName :: Parser Query.OperationName
operationName = spaces *> operationNameP <* spaces

graphQlType :: Parser Query.Type
graphQlType = spaces *> typeP <* spaces

queries :: Parser a -> Parser [a]
queries s = spaces *> many s <* spaces

operationDecl :: String -> Parser Query.OperationName
operationDecl kw = spaces *> try (string kw) *> spaces *> operationName

braces :: Parser a -> Parser a
braces = between (delim '{') (delim '}')

parens :: Parser a -> Parser a
parens = between (delim '(') (delim ')')

delim :: Char -> Parser ()
delim c = spaces *> char c *> spaces

nonnull :: Parser Bool
nonnull = optionBool (delim '!')

optionList :: Parser [a] -> Parser [a]
optionList = option []

optionBool :: Parser a -> Parser Bool
optionBool p = option False (p *> pure True)

dump :: Int -> Parser ()
dump n = do
    s <- getParserState
    let msg = show $ take n (stateInput s)
    trace msg $ return ()
