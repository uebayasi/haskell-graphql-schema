module Data.GraphQL.XXX.Query where

import           Data.Function                ((&))
import           Debug.Trace

import qualified Data.GraphQL.XXX.Query.AST   as Query
import qualified Data.GraphQL.XXX.Schema.AST  as Schema
import qualified Data.GraphQL.XXX.Schema.Info as Info

fixupQuery :: Info.SchemaInfo -> Query.Query -> Query.Query
fixupQuery ai q@(Query.Query qt qfn qargs qfs) =
    Query.Query qt qfn qargs (fixupFields (fromQueryType qt) qfs)
        where
            fixupFields :: Schema.TypeName -> [Query.Field] -> [Query.Field]
            fixupFields stn =
                map (fixupField stn)

            fixupField :: Schema.TypeName -> Query.Field -> Query.Field
            fixupField stn qf@Query.Field{} =
                qf

            {- FieldNodes that don't have child fields are "Field" (not "FieldNode")
            -}
            fixupField stn qf@(Query.FieldNode ffn [] [] _) =
                case lookupSchemaField stn ffn of
                    (Just fi) ->
                        Query.Field ffn ft nn
                        where
                            ft = fromSchemaType (Schema.getFieldType fi)
                            nn = Schema.getFieldNonnull fi
                    _ ->
                        Query.Field ffn Query.String False -- XXX
                            & trace ("fixupField: Field: XXX:\n stn=" ++ show stn ++ "\n qf=" ++ show qf)

            {- FieldNodes that have child fields are "FieldNode" (not "Field")
            -}
            fixupField stn qf@(Query.FieldNode ffn fargs ffs _) =
                case lookupSchemaField stn ffn of
                    (Just fi) ->
                        Query.FieldNode ffn fargs (fixupFields stn' ffs) isList
                        where
                            (stn', isList) =
                                case Schema.getFieldType fi of
                                    Schema.List (Schema.Object utn _ _) -> (utn, True)
                                    Schema.List _ -> (stn, True)
                                    Schema.Object utn _ _ -> (utn, False)
                                    _ -> (stn, False)
                    _ ->
                        Query.FieldNode ffn fargs (fixupFields stn ffs) False
                            & trace ("fixupField: FieldNode: XXX:\n stn=" ++ show stn ++ "\n qf=" ++ show qf)

            lookupSchemaField :: Schema.TypeName -> Query.FieldName -> Maybe Schema.Field
            lookupSchemaField stn =
                Info.lookupField ai stn . fromQueryFieldName

            fromSchemaType :: Schema.Type -> Query.Type
            fromSchemaType s =
                case s of
                    -- XXX
                    -- XXX
                    -- XXX
                    Schema.Boolean -> Query.Boolean
                    Schema.Int     -> Query.Int
                    Schema.String  -> Query.String
                    Schema.Enum (Schema.TypeName n) -> Query.UserType (Query.TypeName n)
                    Schema.Scalar (Schema.TypeName n) _ -> Query.String -- XXX Scalar
                    Schema.Object (Schema.TypeName n) _ _ -> Query.String
                    _ -> Query.String
                        & trace ("XXX fromSchemaType: " ++ show s)
                    -- XXX
                    -- XXX
                    -- XXX

            fromQueryType :: Query.QueryType -> Schema.TypeName
            fromQueryType =
                Schema.TypeName . Query.queryType2name

            fromQueryFieldName :: Query.FieldName -> Schema.FieldName
            fromQueryFieldName =
                Schema.FieldName . Query.getName
