module Generate.Decoder exposing (..)

import String
import Dict
import Generate.Type exposing (findEnums, enumTagName)
import Swagger.Parse as Parse exposing (Definitions, Definition, Definition(Definition), Enum(Enum, NotEnum), Properties, Type(Object', Array', Ref', Int', Float', String', Bool'))
import Codegen.Function as Fun exposing (function, pipeline, letin, caseof)


renderDecoders : Definitions -> String
renderDecoders definitions =
    List.map renderDecoder definitions
        |> List.append (List.filterMap renderEnum (findEnums definitions))
        |> String.concat


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
        String' enum ->
            case enum of
                Enum name enum ->
                    decoderName name

                NotEnum ->
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


renderEnum : Definition -> Maybe String
renderEnum (Definition _ isRequired type') =
    case type' of
        String' (Enum name enum) ->
            Just <|
                function (decoderName name)
                    []
                    ("Decoder " ++ name)
                    (letin
                        [ ( "decodeToType string"
                          , caseof "string"
                                ((List.map (renderEnumEach name) enum) ++ [ renderEnumFail name ])
                          )
                        ]
                        "customDecoder string decodeToType"
                    )

        _ ->
            Nothing


renderEnumEach : String -> String -> ( String, String )
renderEnumEach enumName value =
    ( "\"" ++ value ++ "\"", "Result.Ok " ++ (enumTagName enumName value) )


renderEnumFail : String -> ( String, String )
renderEnumFail enumName =
    ( "_", "Result.Err (\"Invalid value for " ++ enumName ++ ". Value: \" ++ string)" )