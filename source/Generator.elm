module Generator exposing (..)

import String
import Dict
import Regex exposing (regex)
import Json.Decode exposing (decodeString)
import Decoder exposing (Swagger, decodeSwagger, Definition, Property)


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map types


types : Swagger -> String
types { definitions } =
    String.concat <| List.map renderType <| Dict.toList definitions


renderType : ( String, Definition ) -> String
renderType ( name, definition ) =
    "type alias " ++ name ++ " = " ++ (renderTypeFields definition)


renderTypeFields : Definition -> String
renderTypeFields definition =
    "{\n" ++ renderProperties definition ++ "}\n"


renderProperties : Definition -> String
renderProperties { required, properties } =
    let
        required' =
            Maybe.withDefault [] required
    in
        case properties of
            Just properties ->
                String.join ",\n" <| List.filterMap renderProperty <| Dict.toList properties

            Nothing ->
                ""


renderProperty : ( String, Property ) -> Maybe String
renderProperty ( name, Decoder.Property property ) =
    Just <| name ++ " : " ++ renderFieldType (getType property) property


type Type
    = String'
    | Int'
    | Float'
    | Bool'
    | Object'
    | Array'
    | Ref' String
    | Unknown'


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


renderFieldType : Type -> Definition -> String
renderFieldType type' definition =
    case type' of
        String' ->
            "String"

        Int' ->
            "Int"

        Float' ->
            "Float"

        Bool' ->
            "Bool"

        Object' ->
            renderTypeFields definition

        Array' ->
            "TODO (Array)"

        Ref' ref' ->
            renderRefType ref'

        Unknown' ->
            "TODO (Unknown)"


getType : Definition -> Type
getType definition =
    case ( definition.type', definition.ref' ) of
        ( Just type', _ ) ->
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

                "array" ->
                    Array'

                _ ->
                    Unknown'

        ( Nothing, Just ref' ) ->
            Ref' ref'

        ( Nothing, Nothing ) ->
            -- TODO handle property.properties
            -- TODO handle property.items
            Object'
