module Generate.Decoder exposing (..)

import String
import Dict
import Swagger.Decode as Swagger exposing (Swagger, Definition, Property)
import Codegen.Function as Fun exposing (function, pipeline)


type Type
    = String'
    | Int'
    | Float'
    | Bool'
    | Object'
    | Array' Definition
    | Ref' String
    | Unknown'


renderDecoders : Swagger -> String
renderDecoders { definitions } =
    String.concat <| List.map renderDecoder <| Dict.toList definitions


renderDecoder : ( String, Definition ) -> String
renderDecoder ( name, definition ) =
    function (decoderName name)
        []
        ("Decoder " ++ name)
        (renderDecoderBody name definition)


decoderName : String -> String
decoderName name =
    "decode" ++ name


renderDecoderBody : String -> Definition -> String
renderDecoderBody name definition =
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
            renderObjectDecoder name definition

        Array' definition' ->
            "List"

        Ref' ref' ->
            "Ref"

        Unknown' ->
            "TODO (Unknown)"


renderObjectDecoder : String -> Definition -> String
renderObjectDecoder name definition =
    case definition.properties of
        Just properties ->
            Dict.toList properties
                |> List.map renderObjectDecoderProperty
                |> pipeline ("decode " ++ name)

        Nothing ->
            "decode TODO empty object"


renderObjectDecoderProperty : ( String, Property ) -> String
renderObjectDecoderProperty ( name, property ) =
    "optional (\"" ++ name ++ "\" string)"


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
