module Data.GraphQL.XXX.Schema.Info
    ( analyzeSchemaInfo
    , getAnalyzedStatements
    , SchemaInfo(..)
    , emptySchemaInfo
    , lookupField
    ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map                    as Map
import           Data.Maybe
import qualified Data.Set                    as Set
import           Debug.Trace

import           Data.GraphQL.XXX.Schema.AST

data Ctx = Ctx
    { schemaInfo :: SchemaInfo
    , statement  :: Maybe Statement
    , field      :: Maybe Field
    , argument   :: Maybe Argument
    }

emptyCtx :: Ctx
emptyCtx = Ctx emptySchemaInfo Nothing Nothing Nothing

getSchemaInfo :: State Ctx SchemaInfo
getSchemaInfo =
    schemaInfo <$> get

putSchemaInfo :: SchemaInfo -> State Ctx ()
putSchemaInfo ai = do
    ctx <- get
    put ctx
        { schemaInfo = ai
        }

setStatement :: Statement -> State Ctx ()
setStatement g = do
    ctx <- get
    put ctx { statement = Just g }
resetStatement :: State Ctx ()
resetStatement = do
    ctx <- get
    put ctx { statement = Nothing }
setField :: Field -> State Ctx ()
setField f = do
    ctx <- get
    put ctx { field = Just f }
resetField :: State Ctx ()
resetField = do
    ctx <- get
    put ctx { field = Nothing }
setArgument :: Argument -> State Ctx ()
setArgument a = do
    ctx <- get
    put ctx { argument = Just a }
resetArgument :: State Ctx ()
resetArgument = do
    ctx <- get
    put ctx { argument = Nothing }

data SchemaInfo = SchemaInfo
    { getOriginalStatements :: [Statement]
    , getStatements         :: Set.Set Statement
    , getEnumNames          :: Set.Set String
    , getFieldNames         :: Set.Set String
    , getTypeNames          :: Set.Set String
    , getEnums              :: Set.Set Statement
    , getInputs             :: Set.Set Statement
    , getInterfaces         :: Set.Set Statement
    , getObjects            :: Set.Set Statement
    , getScalars            :: Set.Set Statement
    , getUnions             :: Set.Set Statement
    , getRelations          :: [(TypeName, TypeName)]
    , getReferences         :: [(TypeName, TypeName)]
    , getScalarMap          :: [(TypeName, String)]
    } deriving (Show)

emptySchemaInfo :: SchemaInfo
emptySchemaInfo =
    SchemaInfo
    []
    Set.empty
    Set.empty Set.empty Set.empty
    Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty
    [] []
    []

analyzeSchemaInfo :: SchemaInfo -> SchemaInfo
analyzeSchemaInfo info =
    schemaInfo $
        execState f $
            emptyCtx
                { schemaInfo = info
                }
    where
        f = do
            resetStatements
            analyzeStatements
            resetStatements
            analyzeInterfaces
            analyzeRelations
            analyzeObjects

getAnalyzedStatements :: SchemaInfo -> [Statement]
getAnalyzedStatements ai =
    map stmt $ getOriginalStatements ai
        where
            stmt g@EnumDefinition{} =
                lookupEnum_ ai g
            stmt g@InputDefinition{} =
                lookupInput_ ai g
            stmt g@InterfaceDefinition{} =
                lookupInterface_ ai g
            stmt g@ObjectDefinition{} =
                lookupObject_ ai g
            stmt g@ScalarDefinition{} =
                lookupScalar_ ai g
            stmt g = g

{------------------------------------------------------------------------------}

analyzeStatements :: State Ctx ()
analyzeStatements = do
    ai <- getSchemaInfo
    mapM_ (iterateStatement analyzeType) (getOriginalStatements ai)

resetStatements :: State Ctx ()
resetStatements = do
    ai <- getSchemaInfo
    putSchemaInfo emptySchemaInfo
        { getOriginalStatements = getOriginalStatements ai
        , getScalarMap = getScalarMap ai
        }
    mapM_ (iterateStatement reset) (getOriginalStatements ai)

iterateStatement :: (Statement -> State Ctx ()) -> Statement -> State Ctx ()
iterateStatement f g = do
    ai <- getSchemaInfo
    unless (Set.member (getName g) (getTypeNames ai)) $ do
        ai <- getSchemaInfo
        putSchemaInfo ai
            { getStatements = Set.insert g (getStatements ai)
            , getTypeNames = Set.insert (getName g) (getTypeNames ai)
            }
        f g

{------------------------------------------------------------------------------}

class Analyzable a where
    reset :: a -> State Ctx ()
    analyzeType :: a -> State Ctx ()
    analyzeInterface :: a -> State Ctx ()
    analyzeObject :: SchemaInfo -> a -> State Ctx a

instance Analyzable Statement where
    {- reset
    -}
    reset g@(EnumDefinition t es) = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getEnums = Set.insert g (getEnums ai) }

    reset g@(InputDefinition t fs) = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getInputs = Set.insert g (getInputs ai) }

    reset g@(InterfaceDefinition t _ fs) = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getInterfaces = Set.insert g (getInterfaces ai) }

    reset g@(ObjectDefinition t i fs) = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getObjects = Set.insert g (getObjects ai) }

    reset g@(ScalarDefinition t) = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getScalars = Set.insert g (getScalars ai) }

    reset g@(UnionDefinition t ns) = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getUnions = Set.insert g (getUnions ai) }

    {- analyzeType
    -}
    analyzeType g@(EnumDefinition t es) =
        mapM_ analyzeType es

    analyzeType g@(InputDefinition t fs) =
        mapM_ analyzeType fs

    analyzeType g@(InterfaceDefinition t _ fs) =
        mapM_ analyzeType fs

    analyzeType g@(ObjectDefinition t i fs) =
        mapM_ analyzeType fs

    analyzeType g@(ScalarDefinition t) =
        return () -- XXX

    analyzeType g@(UnionDefinition t ns) =
        mapM_ analyzeType ns

    {- analyzeInterface
    -}
    analyzeInterface g@(InterfaceDefinition t _ fs) = do
        ai <- getSchemaInfo
        let
            g' = analyzeInterface' ai g
        putSchemaInfo ai { getInterfaces = Set.insert g' (getInterfaces ai) }

    analyzeInterface _ =
        pure ()

    {- analyzeObject
    -}
    analyzeObject ai g@(InputDefinition t fs) = do
        setStatement g
        g' <- InputDefinition t <$>
            mapM (analyzeObject ai) fs
        resetStatement
        return g'

    analyzeObject ai g@(InterfaceDefinition t ts fs) = do
        setStatement g
        g' <- InterfaceDefinition t ts <$>
            mapM (analyzeObject ai) fs
        resetStatement
        return g'

    analyzeObject ai g@(ObjectDefinition t i fs) = do
        setStatement g
        g' <- ObjectDefinition t i <$>
            mapM (analyzeObject ai) fs
        resetStatement
        return g'

    analyzeObject ai g =
        pure g

instance Analyzable Argument where
    {- reset
    -}
    reset _ =
        pure ()

    {- analyzeType
    -}
    analyzeType (Argument f t b) =
        analyzeType f

    {- analyzeInterface
    -}
    analyzeInterface _ =
        pure ()

    {- analyzeObject
    -}
    analyzeObject ai a@(Argument an at ann) =
        pure $
            Argument an (resetType ai at) ann
            -- Argument an (resetType ai (trace ("resetType: "++show at++" -> "++show (resetType ai at)) at)) ann

instance Analyzable Field where
    {- reset
    -}
    reset _ =
        pure ()

    {- analyzeType
    -}
    analyzeType (Field f as t b _) = do
        mapM_ analyzeType as
        analyzeType f

    {- analyzeInterface
    -}
    analyzeInterface _ =
        pure ()

    {- analyzeObject
    -}
    analyzeObject ai f@(Field fn fargs ft fnn _) = do
        ctx <- get
        fargs' <- mapM (analyzeObject ai) fargs
        let ft' = resetType ai ft
        let frel' = setRelation ai ft' (statement ctx)
        pure $
            Field fn fargs' ft' fnn frel'

instance Analyzable InputField where
    {- reset
    -}
    reset _ =
        pure ()

    {- analyzeType
    -}
    analyzeType (InputField ifn _ _) =
        analyzeType ifn

    {- analyzeInterface
    -}
    analyzeInterface _ =
        pure ()

    {- analyzeObject
    -}
    analyzeObject ai f@(InputField ifn ift ifnn) = do
        let ift' = resetType ai ift
        pure $
            InputField ifn ift' ifnn

instance Analyzable EnumName where
    {- reset
    -}
    reset _ =
        pure ()

    {- analyzeType
    -}
    analyzeType e = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getEnumNames = Set.insert (getName e) (getEnumNames ai) }

    {- analyzeInterface
    -}
    analyzeInterface _ =
        pure ()

    {- analyzeObject
    -}
    analyzeObject ai =
        pure

instance Analyzable FieldName where
    {- reset
    -}
    reset _ =
        pure ()

    {- analyzeType
    -}
    analyzeType f = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getFieldNames = Set.insert (getName f) (getFieldNames ai) }

    {- analyzeInterface
    -}
    analyzeInterface _ =
        pure ()

    {- analyzeObject
    -}
    analyzeObject ai =
        pure

instance Analyzable TypeName where
    {- reset
    -}
    reset _ =
        pure ()

    {- analyzeType
    -}
    analyzeType t = do
        ai <- getSchemaInfo
        putSchemaInfo ai { getTypeNames = Set.insert (getName t) (getTypeNames ai) }

    {- analyzeInterface
    -}
    analyzeInterface _ =
        pure ()

    {- analyzeObject
    -}
    analyzeObject ai =
        pure

{------------------------------------------------------------------------------}

analyzeInterfaces :: State Ctx ()
analyzeInterfaces = do
    ai <- getSchemaInfo
    mapM_ analyzeInterface (getOriginalStatements ai)

analyzeInterface' ai g@(InterfaceDefinition t _ fs) =
    InterfaceDefinition t (interfaceTypes ai t) (collectFields g ai)
        where
            collectFields :: Statement -> SchemaInfo -> [Field]
            collectFields i@(InterfaceDefinition t _ fs) ai =
                mergeFields fs $
                map filterFields $
                filter (isInterfaceOf i) (getOriginalStatements ai)

            filterFields :: Statement -> [Field]
            filterFields (ObjectDefinition xt (Just xi) xfs) = xfs

            mergeFields :: [Field] -> [[Field]] -> [Field]
            mergeFields fs moreFields = fs ++ concatMap (diffFields fs) moreFields

            -- XXX No check
            diffFields :: [Field] -> [Field] -> [Field]
            diffFields o = drop (length o)

interfaceTypes :: SchemaInfo -> TypeName -> [TypeName]
interfaceTypes ai name =
    map (TypeName . getName) $
        filter (isInterfaceOf (InterfaceDefinition name [] [])) $
            getOriginalStatements ai

{------------------------------------------------------------------------------}

analyzeObjects :: State Ctx ()
analyzeObjects = do
    ai <- getSchemaInfo
    os <- mapM (analyzeObject ai) (Set.toList (getObjects ai))
    is <- mapM (analyzeObject ai) (Set.toList (getInterfaces ai))
    putSchemaInfo ai
        { getObjects = Set.fromList os
        , getInterfaces = Set.fromList is
        }
    return ()

resetType :: SchemaInfo -> Type -> Type
resetType ai (List t@Object{}) =
    List (resetType ai t)
resetType ai t@(Object x _ b)
    | isJust (lookupEnum ai x) =
        Enum x
    | isJust (lookupScalar ai x) =
        Scalar
            x
            (fromMaybe "string" (lookupScalarMap ai x)) -- XXX Scalar
    | otherwise =
        Object
            x
            (fromMaybe x (lookupInterfaceByObject ai x))
            b
resetType ai t =
    t

setRelation :: SchemaInfo -> Type -> Maybe Statement -> Maybe Relation
setRelation ai (List (Object u _ _)) mg =
    Just $ Relation
        (lookupRelationForward ai u)
        (do
            g <- mg
            lookupRelation ai u (TypeName $ getName g))
setRelation _ _ _ =
    Nothing

{------------------------------------------------------------------------------}

{-

When:
    type Order {
        bike Bike
    }
    type Bike {
        orders [Order]
    }

- Then there is ai relation of (Order -> Bike).
- Order's table (datastore) records ai pointer (ID string) to Bike.
- Bike's orders are looked up by matching ai Bike ID string against
  the Order table's bike value.

-}

analyzeRelations :: State Ctx ()
analyzeRelations = do
    ai <- getSchemaInfo
    let
        gs = (Set.toList . getObjects) ai
        rs = collectRelations ai gs
        refs = collectReferences ai gs
    putSchemaInfo ai
        { getRelations = rs
        , getReferences = refs
        }

collectRelations :: SchemaInfo -> [Statement] -> [(TypeName, TypeName)]
collectRelations ai gs =
    [ (z ga, z gb)
    | ga <- gs
    , gb <- gs
    , isRelatedForward ga gb
    , isRelatedBackward ga gb
    , isJust (lookupObject ai (z ga))
    , isJust (lookupObject ai (z gb))
    ]
        where
            z = TypeName . getName

collectReferences :: SchemaInfo -> [Statement] -> [(TypeName, TypeName)]
collectReferences ai gs =
    [ (z ga, z gb)
    | ga <- gs
    , gb <- gs
    , isRelatedForward ga gb
    , not (isRelatedBackward ga gb)
    , isJust (lookupObject ai (z ga))
    , isJust (lookupObject ai (z gb))
    , getName ga /= "Query"
    , getName ga /= "Mutation"
    ]
        where
            z = TypeName . getName

-- Order has `bike Bike'
isRelatedForward :: Statement -> Statement -> Bool
isRelatedForward (ObjectDefinition _ _ fsa) (ObjectDefinition tb _ _) =
    (not . null) $ filter p fsa
        where
            p (Field _ _ (Object ua _ _) _ _) =
                getName tb == getName ua
            p _ =
                False

-- Bike has `orders [Order]'
isRelatedBackward :: Statement -> Statement -> Bool
isRelatedBackward (ObjectDefinition ta _ _) (ObjectDefinition _ _ fsb) =
    (not . null) $ filter p fsb
        where
            p (Field _ _ (List (Object ub _ _)) _ _) =
                getName ta == getName ub
            p _ =
                False

{------------------------------------------------------------------------------}

lookupInput :: SchemaInfo -> TypeName -> Maybe Statement
lookupInput ai t = do
    idx <- Set.lookupIndex (InputDefinition t []) (getInputs ai)
    return $ Set.elemAt idx (getInputs ai)
lookupInput_ ai g =
    fromMaybe g (lookupInput ai (TypeName $ getName g))

lookupObject :: SchemaInfo -> TypeName -> Maybe Statement
lookupObject ai t = do
    idx <- Set.lookupIndex (ObjectDefinition t Nothing []) (getObjects ai)
    return $ Set.elemAt idx (getObjects ai)
lookupObject_ ai g =
    fromMaybe g (lookupObject ai (TypeName $ getName g))

lookupInterface :: SchemaInfo -> TypeName -> Maybe Statement
lookupInterface ai t = do
    idx <- Set.lookupIndex (InterfaceDefinition t [] []) (getInterfaces ai)
    return $ Set.elemAt idx (getInterfaces ai)
lookupInterface_ ai g =
    fromMaybe g (lookupInterface ai (TypeName $ getName g))

lookupInterfaceByObject :: SchemaInfo -> TypeName -> Maybe TypeName
lookupInterfaceByObject ai t = do
    (ObjectDefinition _ i _) <- lookupObject ai t
    i

lookupEnum :: SchemaInfo -> TypeName -> Maybe Statement
lookupEnum ai t = do
    idx <- Set.lookupIndex (EnumDefinition t []) (getEnums ai)
    return $ Set.elemAt idx (getEnums ai)
lookupEnum_ ai g =
    fromMaybe g (lookupEnum ai (TypeName $ getName g))

lookupScalar :: SchemaInfo -> TypeName -> Maybe Statement
lookupScalar ai t = do
    idx <- Set.lookupIndex (ScalarDefinition t) (getScalars ai)
    return $ Set.elemAt idx (getScalars ai)
lookupScalar_ ai g =
    fromMaybe g (lookupScalar ai (TypeName $ getName g))

lookupScalarMap :: SchemaInfo -> TypeName -> Maybe String
lookupScalarMap ai x =
    Map.lookup x $
        Map.fromList (getScalarMap ai) -- XXX Scalar

lookupRelationForward :: SchemaInfo -> TypeName -> Maybe TypeName
lookupRelationForward ai i =
    let
        is = interfaceTypes ai i
        ts =
            [ snd r
            | i <- if null is then [i] else is
            , r <- getRelations ai
            , i == fst r
            ]
    in
        if null ts
            then Nothing
            else Just $ head ts

lookupRelation :: SchemaInfo -> TypeName -> TypeName -> Maybe TypeName
lookupRelation ai u i =
    let
        is = interfaceTypes ai i
        ts =
            [ snd r
            | i <- if null is then [i] else is
            , r <- getRelations ai
            , u == fst r
            , i == snd r
            ]
    in
        if null ts
            then Nothing
            else Just $ head ts

lookupReference :: SchemaInfo -> TypeName -> Maybe TypeName
lookupReference ai i =
    let
        is = interfaceTypes ai i
        ts =
            [ snd r
            | i <- if null is then [i] else is
            , r <- getReferences ai
            , i == fst r
            ]
    in
        if null ts
            then Nothing
            else Just $ head ts

lookupField :: SchemaInfo -> TypeName -> FieldName -> Maybe Field
lookupField ai tn fn = do
    (ObjectDefinition _ i fs) <- lookupObject ai tn
    idx <- Set.lookupIndex
            (Field fn [] Int False Nothing)
            (Set.fromList fs)
    Just (Set.elemAt idx (Set.fromList fs))
