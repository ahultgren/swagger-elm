module Generator exposing (..)

import String
import Dict
import Json.Decode exposing (decodeString)
import Decoder exposing (Swagger, decodeSwagger, SwaggerDefinition, SwaggerDefinitionProperty)


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map types


types : Swagger -> String
types { definitions } =
    String.concat <| List.map renderType <| Dict.toList definitions


renderType : ( String, SwaggerDefinition ) -> String
renderType ( name, definition ) =
    "type alias " ++ name ++ " = {\n" ++ renderProperties definition ++ "}\n"


renderProperties : SwaggerDefinition -> String
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


renderProperty : ( String, SwaggerDefinitionProperty ) -> Maybe String
renderProperty ( name, property ) =
    Just <| name ++ " : " ++ renderFieldType (getType property) property


type Type
    = String'
    | Int'
    | Float'
    | Bool'
    | Object'
    | Array'
    | Ref'
    | Unknown'


renderFieldType : Type -> SwaggerDefinitionProperty -> String
renderFieldType type' property =
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
            "TODO (implement objects as property)"

        Array' ->
            "TODO (implement objects as property)"

        Ref' ->
            "TODO Ref"

        Unknown' ->
            "TODO (implement unknown property (or crash))"


getType : SwaggerDefinitionProperty -> Type
getType property =
    case property.type' of
        Just type' ->
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

        Nothing ->
            case property.ref' of
                Just ref' ->
                    Ref'

                Nothing ->
                    -- TODO handle property.items
                    Object'
