module Data.GraphQL.XXX.Schema.AST
    ( Statement(..)
    , Argument(..)
    , Field(..)
    , InputField(..)
    , Relation(..)
    , Arguments
    , Fields
    , InputFields
    , EnumName(..)
    , FieldName(..)
    , TypeName(..)
    , Type(..)
    , EnumNames
    , TypeNames
    , Typed(..)
    , Named(..)
    , isInterfaceOf
    ) where

{- Statement -}

data Statement
    = EnumDefinition TypeName EnumNames
    | InputDefinition TypeName InputFields
    | InterfaceDefinition TypeName [TypeName] Fields
    | ObjectDefinition TypeName (Maybe TypeName) Fields
    | ScalarDefinition TypeName
    | UnionDefinition TypeName TypeNames
    deriving (Show)

instance Eq Statement where
    a == b = getName a == getName b

instance Ord Statement where
    compare a b = compare (getName a) (getName b)

{- Argument, Field -}

data Argument =
    Argument FieldName Type Bool
        deriving (Show)
data Field = Field
    { getFieldFieldName :: FieldName
    , getFieldArguments :: Arguments
    , getFieldType      :: Type
    , getFieldNonnull   :: Bool
    , getFieldRelation  :: Maybe Relation -- XXX See below
    } deriving (Show)

instance Eq Field where
    a == b = getName a == getName b
instance Ord Field where
    compare a b = compare (getName a) (getName b)

data InputField = InputField
    { getInputFieldName    :: FieldName
    , getInputFieldType    :: Type
    , getInputFieldNonnull :: Bool
    } deriving (Show)

instance Eq InputField where
    a == b = getName a == getName b
instance Ord InputField where
    compare a b = compare (getName a) (getName b)

type Arguments = [Argument]
type Fields = [Field]
type InputFields = [InputField]

-- XXX
-- XXX Reconsider design ... one day
-- XXX
data Relation = Relation
    { forward  :: Maybe TypeName
    , backward :: Maybe TypeName
    }
    deriving (Show)
-- XXX
-- XXX
-- XXX

{- EnumName, FieldName, TypeName -}

newtype EnumName = EnumName {
    getEnumName :: String
} deriving (Show)
newtype FieldName = FieldName {
    getFieldName :: String
} deriving (Show)
data TypeName
    = TypeNameQuery -- type Query
    | TypeNameMutation -- type Mutation
    | TypeName {
        getTypeName :: String
    }
    deriving (Show)

instance Eq FieldName where
    a == b = getName a == getName b
instance Ord FieldName where
    compare a b = compare (getName a) (getName b)

instance Eq TypeName where
    a == b = getName a == getName b
instance Ord TypeName where
    compare a b = compare (getName a) (getName b)

type EnumNames = [EnumName]
type TypeNames = [TypeName]

{- Type -}

data Type
    = Boolean
    | Float
    | List Type
    | ID
    | Int
    | String
    | Object TypeName TypeName Bool
    | Enum TypeName
    | Scalar TypeName String -- String == field name (e.g. "time.Time")
    -- XXX | Input TypeName [Field]
    deriving (Show)

{- Scalar

- Given: "scalar Time"
- Name
  - "Time"
- Types
  - String of RFC3339 in JSON
  - time.Time in Go/Datastore
    - This information can't be described in schema
      - XXX Hard-coded in server/client
    - Server
- Server
  - Internal mappings of type names ("Time") and their internal types (time.Time)

-}

{- Input

XXX Input object can't have Arguments

-}

{------------------------------------------------------------------------------}

class Typed a where
    getType :: a -> TypeName

instance Typed Statement where
    getType (EnumDefinition t _)        = t
    getType (InputDefinition t _)       = t
    getType (InterfaceDefinition t _ _) = t
    getType (ObjectDefinition t _ _)    = t
    getType (ScalarDefinition t)        = t
    getType (UnionDefinition t _)       = t

class Named a where
    getName :: a -> String

instance Named Statement where
    getName = getName . getType

instance Named Field where
    getName = getName . getFieldFieldName

instance Named InputField where
    getName = getName . getInputFieldName

instance Named EnumName where
    getName = getEnumName

instance Named FieldName where
    getName = getFieldName

instance Named TypeName where
    getName TypeNameQuery    = "Query"
    getName TypeNameMutation = "Mutation"
    getName t@(TypeName _)   = getTypeName t

{------------------------------------------------------------------------------}

isInterfaceOf :: Statement -> Statement -> Bool
isInterfaceOf i o@(ObjectDefinition xt (Just xi) xfs) =
    getName i == getName xi
isInterfaceOf _ _ = False
