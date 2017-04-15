module Generate.Decoder exposing (..)

import Codegen.Function as Fun exposing (function, pipeline, letin, caseof)
import Codegen.Utils exposing (capitalize, sanitize)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, String_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional)
        )


renderDecoder : Definition -> String
renderDecoder definition =
    let
        name =
            getFullName definition
    in
        function (decoderName <| name)
            []
            ("Decoder " ++ (capitalize <| sanitize <| name))
            (renderDecoderBody <| getType definition)


decoderName : String -> String
decoderName name =
    "decode" ++ (capitalize <| sanitize name)


renderDecoderBody : Type -> String
renderDecoderBody type_ =
    case type_ of
        Object_ properties ->
            renderObjectBody properties

        Array_ items ->
            toString items

        String_ default enum ->
            toString default

        Int_ default ->
            renderPrimitiveBody "int" default

        Float_ default ->
            renderPrimitiveBody "float" default

        Bool_ default ->
            renderPrimitiveBody "bool" default

        Ref_ ref ->
            decoderName ref


renderPrimitiveBody : String -> Maybe String -> String
renderPrimitiveBody type_ default =
    case default of
        Nothing ->
            type_

        Just default ->
            -- TODO: What to do here?
            type_


renderObjectBody : Properties -> String
renderObjectBody (Properties properties) =
    properties
        |> List.map renderObjectDecoderProperty
        |> pipeline ("decode " ++ "todo")


renderObjectDecoderProperty : Property -> String
renderObjectDecoderProperty property =
    case property of
        Required name type_ ->
            "required \"" ++ name ++ "\" " ++ "todo"

        Optional name type_ ->
            "optional \"" ++ name ++ "\" " ++ "todo"
