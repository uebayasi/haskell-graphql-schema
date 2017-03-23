module Data.GraphQL.XXX.Query where

import           Debug.Trace

import qualified Data.GraphQL.XXX.Query.AST   as Query
import qualified Data.GraphQL.XXX.Schema.AST  as Schema
import qualified Data.GraphQL.XXX.Schema.Info as Info

fixupQuery :: Info.SchemaInfo -> Query.Query -> Query.Query
fixupQuery ai q@(Query.Query qt qfn qargs qfs) =
    Query.Query qt qfn qargs (fixupFields rootField (queryType2SchemaType qt) qfs)
        where
            rootField :: Query.Field
            rootField = Query.FieldNode (Query.FieldName (Query.queryType2name qt)) [] qfs False

            fixupFields :: Query.Field -> Schema.TypeName -> [Query.Field] -> [Query.Field]
            fixupFields pf stn =
                map (fixupField pf stn)

            fixupField :: Query.Field -> Schema.TypeName -> Query.Field -> Query.Field
            fixupField pf stn qf@Query.Field{} =
                qf

            fixupField pf stn qf@(Query.FieldNode ffn [] [] _) =
                case lookupSchemaField ai stn ffn of
                    (Just fi) ->
                        --trace ("fixupField: Field:\n pf=" ++ show pf ++ "\n stn=" ++ show stn ++ "\n qf=" ++ show (Query.Field ffn ft nn))
                        Query.Field ffn ft nn
                        where
                            ft = schemaType2QueryType (Schema.getFieldType fi)
                            --ft = schemaType2QueryType (trace ("fi=" ++ show (Schema.getFieldType fi)) (Schema.getFieldType fi))
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

            fixupField pf stn qf@(Query.FieldNode ffn fargs ffs _) =
                case lookupSchemaField ai stn ffn of
                    (Just fi) ->
                        case Schema.getFieldType fi of
                            Schema.List (Schema.Object utn _ _) ->
                                Query.FieldNode ffn fargs (fixupFields pf utn ffs) True
                            Schema.List _ ->
                                Query.FieldNode ffn fargs (fixupFields pf stn ffs) True
                            Schema.Object utn _ _ ->
                                Query.FieldNode ffn fargs (fixupFields pf utn ffs) False
                            _ ->
                                Query.FieldNode ffn fargs (fixupFields pf stn ffs) False
                    _ ->
                        -- XXX
                        -- XXX
                        -- XXX
                        trace ("fixupField: FieldNode: XXX:\n pf=" ++ show pf ++ "\n stn=" ++ show stn ++ "\n qf=" ++ show qf)
                            Query.FieldNode ffn fargs (fixupFields pf stn ffs) False
                        -- XXX
                        -- XXX
                        -- XXX

            lookupSchemaField :: Info.SchemaInfo -> Schema.TypeName -> Query.FieldName -> Maybe Schema.Field
            lookupSchemaField ai stn qfn =
                Info.lookupField ai stn (Schema.FieldName (Query.getName qfn))

            schemaType2QueryType :: Schema.Type -> Query.Type
            schemaType2QueryType s =
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
                    _ -> trace ("XXX schemaType2QueryType: " ++ show s) Query.String
                    -- XXX
                    -- XXX
                    -- XXX

            queryType2SchemaType :: Query.QueryType -> Schema.TypeName
            queryType2SchemaType =
                Schema.TypeName . Query.queryType2name
