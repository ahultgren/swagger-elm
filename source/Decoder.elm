module Decoder exposing (..)

import String
import Dict
import Swagger exposing (Swagger, Definition, Property)
import Type exposing (getType, Type)


renderDecoders : Swagger -> String
renderDecoders { definitions } =
    String.concat <| List.map renderDecoder <| Dict.toList definitions


renderDecoder : ( String, Definition ) -> String
renderDecoder ( name, definition ) =
    renderDecoderHeader name
        ++ decoderName name
        ++ " = "
        ++ renderDecoderBody name definition
        ++ "\n"


renderDecoderHeader : String -> String
renderDecoderHeader name =
    decoderName name ++ " : Decoder " ++ name ++ "\n"


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
            "decode "
                ++ name
                ++ "\n"
                ++ (String.join "\n  " <| List.map renderObjectDecoderProperty <| Dict.toList properties)

        Nothing ->
            "decode TODO empty object"


renderObjectDecoderProperty : ( String, Property ) -> String
renderObjectDecoderProperty ( name, property ) =
    let
        x =
            Debug.log "mjau" property
    in
        "|> optional (\"" ++ name ++ "\" string)"
