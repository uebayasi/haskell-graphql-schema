module Data.GraphQL.XXX.Schema.AST
  ( GraphQLStatement(..)
  , GraphQLArgument(..)
  , GraphQLField(..)
  , GraphQLArguments
  , GraphQLFields
  , GraphQLEnumName(..)
  , GraphQLFieldName(..)
  , GraphQLTypeName(..)
  , GraphQLType(..)
  , GraphQLEnumNames
  , GraphQLTypeNames
  ) where

data GraphQLStatement
  = EnumDefinition GraphQLTypeName GraphQLEnumNames
  | InputDefinition GraphQLTypeName GraphQLFields
  | InterfaceDefinition GraphQLTypeName GraphQLFields
  | ObjectDefinition GraphQLTypeName (Maybe GraphQLTypeName) GraphQLFields
  | ScalarDefinition GraphQLTypeName
  | UnionDefinition GraphQLTypeName GraphQLTypeNames
  deriving (Show)

data GraphQLArgument = GraphQLArgument GraphQLFieldName GraphQLType deriving (Show)
data GraphQLField = GraphQLField GraphQLFieldName GraphQLArguments GraphQLType Bool deriving (Show)

type GraphQLArguments = [GraphQLArgument]
type GraphQLFields = [GraphQLField]

data GraphQLEnumName = GraphQLEnumName String deriving (Show)
data GraphQLFieldName = GraphQLFieldName String deriving (Show)
data GraphQLTypeName = GraphQLTypeName String deriving (Show)

type GraphQLEnumNames = [GraphQLEnumName]
type GraphQLTypeNames = [GraphQLTypeName]

data GraphQLType
  = GraphQLBoolean
  | GraphQLFloat
  | GraphQLList GraphQLType
  | GraphQLID
  | GraphQLInt
  | GraphQLString
  | GraphQLUserType GraphQLTypeName
  deriving (Show)
