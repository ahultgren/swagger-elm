module Generator exposing (..)

import String
import Dict
import Regex exposing (regex)
import Json.Decode exposing (decodeString)
import Decoder exposing (Swagger, decodeSwagger, Definition, Property)


type Type
    = String'
    | Int'
    | Float'
    | Bool'
    | Object'
    | Array' Definition
    | Ref' String
    | Unknown'


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map types


types : Swagger -> String
types { definitions } =
    String.concat <| List.map renderType <| Dict.toList definitions


renderType : ( String, Definition ) -> String
renderType ( name, definition ) =
    "type alias " ++ name ++ " = " ++ (renderFieldType definition) ++ "\n"


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
    Just <| name ++ " : " ++ renderFieldType property


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


renderFieldType : Definition -> String
renderFieldType definition =
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
            "{\n" ++ renderProperties definition ++ "}\n"

        Array' definition ->
            "List " ++ (renderFieldType definition)

        Ref' ref' ->
            renderRefType ref'

        Unknown' ->
            "TODO (Unknown)"


getType : Definition -> Type
getType { type', ref', items } =
    case ( type', ref', items ) of
        ( Just "array", _, Just (Decoder.Property items') ) ->
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

        ( Nothing, Nothing, Just (Decoder.Property items') ) ->
            Array' items'

        ( Nothing, Nothing, Nothing ) ->
            Object'
