module Generate.Decoder exposing (..)

import String
import Dict
import Swagger.Decode as Swagger exposing (Swagger, Definition, Property)
import Generate.Type as Type exposing (getType, Type)
import Codegen.Function as Fun exposing (function, pipeline)


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
        Type.String' ->
            "String"

        Type.Int' ->
            "Int"

        Type.Float' ->
            "Float"

        Type.Bool' ->
            "Bool"

        Type.Object' ->
            renderObjectDecoder name definition

        Type.Array' definition' ->
            "List"

        Type.Ref' ref' ->
            "Ref"

        Type.Unknown' ->
            "TODO (Unknown)"


renderObjectDecoder : String -> Definition -> String
renderObjectDecoder name definition =
    case definition.properties of
        Just properties ->
            Dict.toList properties
                |> List.map renderObjectDecoderProperty
                |> pipeline (decoderName name)

        Nothing ->
            "decode TODO empty object"


renderObjectDecoderProperty : ( String, Property ) -> String
renderObjectDecoderProperty ( name, property ) =
    let
        x =
            Debug.log "mjau" property
    in
        "optional (\"" ++ name ++ "\" string)"
