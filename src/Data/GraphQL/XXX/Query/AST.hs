module Data.GraphQL.XXX.Query.AST where

data Query
    = Query QueryType OperationName [QueryArgument] [Field]
    deriving (Show)

emptyQuery :: Query
emptyQuery = Query QueryTypeQuery (OperationName "") [] []

data QueryType
    = QueryTypeQuery
    | QueryTypeMutation
    | QueryTypeIntrospection
    | QueryTypeUser
    deriving (Show)

queryType2name qt =
    case qt of
        QueryTypeQuery         -> "Query"
        QueryTypeMutation      -> "Mutation"
        QueryTypeIntrospection -> "Introspection"

data QueryArgument
    = QueryArgument VarName Type Bool
    deriving (Show)

emptyQueryArgument = QueryArgument (VarName "") Boolean False

data Field
    = Field FieldName Type Bool -- XXX Type and Bool must be read from schema
    | FieldNode FieldName [FieldArgument] [Field] Bool -- Bool (== "isList") must be read from schema
    deriving (Show)

emptyField = Field (FieldName "") Boolean False

data FieldArgument
    = FieldArgument FieldName VarName
    | FieldArgumentInput FieldName [FieldArgument]
    deriving (Show)

newtype FieldName = FieldName String
    deriving (Show)

newtype VarName = VarName String
    deriving (Show)

newtype TypeName = TypeName String
    deriving (Show)

newtype OperationName = OperationName String
    deriving (Show)

newtype EnumName = EnumName String
    deriving (Show)

data Type
    = Boolean
    | Float
    | List Type
    | ID
    | Int
    | String
    | UserType TypeName
    deriving (Show)

{------------------------------------------------------------------------------}

class Named a where
    getName :: a -> String

instance Named Query where
    getName (Query _ f _ _) = getName f

instance Named Field where
    getName (Field f _ _)       = getName f
    getName (FieldNode f _ _ _) = getName f

instance Named FieldName where
    getName (FieldName n) = n
instance Named VarName where
    getName (VarName n) = n
instance Named TypeName where
    getName (TypeName n) = n
instance Named OperationName where
    getName (OperationName n) = n
