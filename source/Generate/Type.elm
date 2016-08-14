module Generate.Type exposing (..)

import String
import Dict
import Regex exposing (regex)
import Swagger.Decode as Swagger exposing (Swagger, Definition, Property)
import Codegen.Type exposing (typeAlias, record, recordField, list, maybe)


type Type
    = String'
    | Int'
    | Float'
    | Bool'
    | Object'
    | Array' Definition
    | Ref' String
    | Unknown'


renderTypes : Swagger -> String
renderTypes { definitions } =
    String.concat <| List.map renderType <| Dict.toList definitions


renderType : ( String, Definition ) -> String
renderType ( name, definition ) =
    typeAlias name (renderFieldType True definition)


renderRefType : String -> String
renderRefType ref =
    let
        parsed =
            (List.head (Regex.find (Regex.AtMost 1) (regex "^#/definitions/(.+)$") ref))
                `Maybe.andThen` (List.head << .submatches)
    in
        case parsed of
            Just (Just ref') ->
                ref'

            _ ->
                Debug.crash "Unparseable reference " ++ ref


renderFieldType : Bool -> Definition -> String
renderFieldType isRequired' definition =
    let
        type' =
            case getType definition of
                String' ->
                    "String"

                Int' ->
                    "Int"

                Float' ->
                    "Float"

                Bool' ->
                    "Bool"

                Object' ->
                    record <| renderProperties definition

                Array' definition' ->
                    -- TODO How to check if the array is required?
                    list <| renderFieldType True definition'

                Ref' ref' ->
                    renderRefType ref'

                Unknown' ->
                    "TODO (Unknown)"
    in
        maybeWrap isRequired' type'


getType : Definition -> Type
getType { type', ref', items } =
    case ( type', ref', items ) of
        ( Just "array", _, Just (Swagger.Property items') ) ->
            Array' items'

        ( Just type', _, _ ) ->
            case type' of
                "string" ->
                    String'

                "integer" ->
                    Int'

                "number" ->
                    Float'

                "boolean" ->
                    Bool'

                "object" ->
                    Object'

                _ ->
                    Unknown'

        ( Nothing, Just ref', _ ) ->
            Ref' ref'

        ( Nothing, Nothing, Just (Swagger.Property items') ) ->
            Array' items'

        ( Nothing, Nothing, Nothing ) ->
            Object'


renderProperties : Definition -> List String
renderProperties { required, properties } =
    case properties of
        Just properties ->
            List.filterMap (renderProperty required) <| Dict.toList properties

        Nothing ->
            []


renderProperty : List String -> ( String, Property ) -> Maybe String
renderProperty required ( name, Swagger.Property property ) =
    Just <| recordField name <| renderFieldType (isRequired required name) property


maybeWrap : Bool -> String -> String
maybeWrap isRequired type' =
    if isRequired then
        type'
    else
        maybe type'


isRequired : List String -> String -> Bool
isRequired required name =
    List.member name required
