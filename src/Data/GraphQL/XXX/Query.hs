module Data.GraphQL.XXX.Query where

import           Debug.Trace

import qualified Data.GraphQL.XXX.Query.AST   as Query
import qualified Data.GraphQL.XXX.Schema.AST  as Schema
import qualified Data.GraphQL.XXX.Schema.Info as Info

fixupQuery :: Info.SchemaInfo -> Query.Query -> Query.Query
fixupQuery ai q@(Query.Query qt qfn qargs qfs) =
    Query.Query qt qfn qargs (fixupFields (fromQueryType qt) rootField qfs)
        where
            rootField :: Query.Field
            rootField = Query.FieldNode (Query.FieldName (Query.queryType2name qt)) [] qfs False

            fixupFields :: Schema.TypeName -> Query.Field -> [Query.Field] -> [Query.Field]
            fixupFields stn pf =
                map (fixupField stn pf)

            fixupField :: Schema.TypeName -> Query.Field -> Query.Field -> Query.Field
            fixupField stn pf qf@Query.Field{} =
                qf

            fixupField stn pf qf@(Query.FieldNode ffn [] [] _) =
                case lookupSchemaField ai stn ffn of
                    (Just fi) ->
                        --trace ("fixupField: Field:\n pf=" ++ show pf ++ "\n stn=" ++ show stn ++ "\n qf=" ++ show (Query.Field ffn ft nn))
                        Query.Field ffn ft nn
                        where
                            ft = fromSchemaType (Schema.getFieldType fi)
                            --ft = fromSchemaType (trace ("fi=" ++ show (Schema.getFieldType fi)) (Schema.getFieldType fi))
                            nn = Schema.getFieldNonnull fi
                    _ ->
                        -- XXX
                        -- XXX
                        -- XXX
                        trace ("fixupField: Field: XXX:\n pf=" ++ show pf ++ "\n stn=" ++ show stn ++ "\n qf=" ++ show qf)
                            Query.Field ffn Query.String False -- XXX
                        -- XXX
                        -- XXX
                        -- XXX

            fixupField stn pf qf@(Query.FieldNode ffn fargs ffs _) =
                case lookupSchemaField ai stn ffn of
                    (Just fi) ->
                        case Schema.getFieldType fi of
                            Schema.List (Schema.Object utn _ _) ->
                                Query.FieldNode ffn fargs (fixupFields utn pf ffs) True
                            Schema.List _ ->
                                Query.FieldNode ffn fargs (fixupFields stn pf ffs) True
                            Schema.Object utn _ _ ->
                                Query.FieldNode ffn fargs (fixupFields utn pf ffs) False
                            _ ->
                                Query.FieldNode ffn fargs (fixupFields stn pf ffs) False
                    _ ->
                        -- XXX
                        -- XXX
                        -- XXX
                        trace ("fixupField: FieldNode: XXX:\n pf=" ++ show pf ++ "\n stn=" ++ show stn ++ "\n qf=" ++ show qf)
                            Query.FieldNode ffn fargs (fixupFields stn pf ffs) False
                        -- XXX
                        -- XXX
                        -- XXX

            lookupSchemaField :: Info.SchemaInfo -> Schema.TypeName -> Query.FieldName -> Maybe Schema.Field
            lookupSchemaField ai stn qfn =
                Info.lookupField ai stn (Schema.FieldName (Query.getName qfn))

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
                    _ -> trace ("XXX fromSchemaType: " ++ show s) Query.String
                    -- XXX
                    -- XXX
                    -- XXX

            fromQueryType :: Query.QueryType -> Schema.TypeName
            fromQueryType =
                Schema.TypeName . Query.queryType2name
