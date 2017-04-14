module Generate.Decoder exposing (..)

import String
import Json.Decode exposing (decodeString, string)
import Generate.Type exposing (findEnums, enumTagName)
import Swagger.Parse as Parse
    exposing
        ( Definitions
        , Definition
        , Definition(Definition)
        , Enum(Enum, NotEnum)
        , IsRequired(IsRequired, NotRequired)
        , Properties
        , Type(Object_, Array_, Ref_, Int_, Float_, String_, Bool_)
        )
import Codegen.Function as Fun exposing (function, pipeline, letin, caseof)
import Codegen.Utils exposing (capitalize, sanitize)


renderDecoders : Definitions -> String
renderDecoders definitions =
    List.map renderDecoder definitions
        |> List.append (List.filterMap renderEnum (findEnums definitions))
        |> String.concat


renderDecoder : Definition -> String
renderDecoder (Definition name isRequired type_) =
    let
        safeName =
            sanitize name
    in
        function (decoderName safeName)
            []
            ("Decoder " ++ (capitalize safeName))
            (renderDecoderBody safeName type_)


decoderName : String -> String
decoderName name =
    "decode" ++ (capitalize name)


renderDecoderBody : String -> Type -> String
renderDecoderBody constructor type_ =
    case type_ of
        String_ enum ->
            case enum of
                Enum name enum ->
                    decoderName <| sanitize name

                NotEnum ->
                    "string"

        Int_ ->
            "int"

        Float_ ->
            "float"

        Bool_ ->
            "bool"

        Object_ properties ->
            renderObjectDecoder constructor properties

        Array_ definition ->
            renderListDecoder definition

        Ref_ ref_ ->
            decoderName <| sanitize ref_


renderObjectDecoder : String -> Properties -> String
renderObjectDecoder safeName properties =
    properties
        |> List.map renderObjectDecoderProperty
        |> pipeline ("decode " ++ (capitalize safeName))


renderObjectDecoderProperty : Definition -> String
renderObjectDecoderProperty (Definition name isRequired type_) =
    maybeDefaultWrap isRequired type_ <| " \"" ++ name ++ "\" " ++ (renderDecoderBody (sanitize name) type_)


maybeDefaultWrap : IsRequired -> Type -> String -> String
maybeDefaultWrap isRequired type_ =
    case isRequired of
        IsRequired ->
            (++) "required"

        NotRequired default ->
            case default of
                Just default ->
                    (++) "optional" << (flip (++)) (" " ++ defaultValue type_ default)

                Nothing ->
                    (++) "maybe"


defaultValue : Type -> String -> String
defaultValue type_ default =
    case type_ of
        String_ (Enum name enum) ->
            case decodeString string default of
                Ok default ->
                    (enumTagName name default)

                Err err ->
                    Debug.crash "Invalid default value" err default

        _ ->
            default


renderListDecoder : Definition -> String
renderListDecoder (Definition name isRequired type_) =
    "(list (" ++ (renderDecoderBody name type_) ++ "))"


renderEnum : Definition -> Maybe String
renderEnum (Definition _ isRequired type_) =
    case type_ of
        String_ (Enum name enum) ->
            let
                safeName =
                    sanitize name
            in
                Just <|
                    function (decoderName safeName)
                        []
                        ("Decoder " ++ safeName)
                        (letin
                            [ ( "decodeToType string"
                              , caseof "string"
                                    ((List.map (renderEnumEach safeName) enum) ++ [ renderEnumFail safeName ])
                              )
                            ]
                            "customDecoder string decodeToType"
                        )

        _ ->
            Nothing


renderEnumEach : String -> String -> ( String, String )
renderEnumEach enumName value =
    ( "\"" ++ value ++ "\"", "Result.Ok " ++ (sanitize <| enumTagName enumName value) )


renderEnumFail : String -> ( String, String )
renderEnumFail enumName =
    ( "_", "Result.Err (\"Invalid value for " ++ enumName ++ ". Value: \" ++ string)" )
