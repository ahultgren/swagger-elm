module Generate.Decoder exposing (..)

import String
import Dict
import Swagger.Parse as Parse exposing (Definitions, Definition, Definition(Definition), Properties, Type(Object', Array', Ref', Int', Float', String', Bool'))
import Codegen.Function as Fun exposing (function, pipeline)


renderDecoders : Definitions -> String
renderDecoders definitions =
    String.concat <| List.map renderDecoder definitions


renderDecoder : Definition -> String
renderDecoder (Definition name isRequired type') =
    function (decoderName name)
        []
        ("Decoder " ++ name)
        (renderDecoderBody name type')


decoderName : String -> String
decoderName name =
    "decode" ++ name


renderDecoderBody : String -> Type -> String
renderDecoderBody constructor type' =
    case type' of
        String' ->
            "string"

        Int' ->
            "int"

        Float' ->
            "float"

        Bool' ->
            "bool"

        Object' properties ->
            renderObjectDecoder constructor properties

        Array' definition ->
            renderListDecoder definition

        Ref' ref' ->
            decoderName ref'


renderObjectDecoder : String -> Properties -> String
renderObjectDecoder name properties =
    properties
        |> List.map renderObjectDecoderProperty
        |> pipeline ("decode " ++ name)


renderObjectDecoderProperty : Definition -> String
renderObjectDecoderProperty (Definition name isRequired type') =
    let
        callee =
            if isRequired then
                "required"
            else
                "maybe"
    in
        callee ++ " \"" ++ name ++ "\" " ++ (renderDecoderBody name type')


renderListDecoder : Definition -> String
renderListDecoder (Definition name isRequired type') =
    "(list (" ++ (renderDecoderBody name type') ++ "))"
