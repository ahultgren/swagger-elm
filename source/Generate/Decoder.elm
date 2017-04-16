module Generate.Decoder exposing (..)

import Generate.Utils exposing (typeName, decoderName, nestedDecoderName)
import Codegen.Function as Fun exposing (function, pipeline, letin, caseof)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, String_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional)
        , getItemsType
        )


renderDecoder : Definition -> String
renderDecoder definition =
    let
        name =
            getFullName definition
    in
        function (decoderName <| name)
            []
            ("Decoder " ++ typeName name)
            (renderDecoderBody definition)


renderDecoderBody : Definition -> String
renderDecoderBody definition =
    case getType definition of
        Object_ properties ->
            renderObjectBody (getFullName definition) properties

        Array_ items ->
            renderArrayBody (getFullName definition) (getItemsType items)

        String_ default enum ->
            -- TODO: handle enum
            renderPrimitiveBody "string" default

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


renderArrayBody : String -> Type -> String
renderArrayBody name type_ =
    "list " ++ (renderPropertyDecoder name "Item" type_)


renderObjectBody : String -> Properties -> String
renderObjectBody name (Properties properties) =
    properties
        |> List.map (renderObjectDecoderProperty name)
        |> pipeline ((++) "decode " <| typeName name)


renderObjectDecoderProperty : String -> Property -> String
renderObjectDecoderProperty parentName property =
    case property of
        Required name type_ ->
            "required \"" ++ name ++ "\" " ++ renderPropertyDecoder parentName name type_

        Optional name type_ ->
            "maybe \"" ++ name ++ "\" " ++ renderPropertyDecoder parentName name type_


renderPropertyDecoder : String -> String -> Type -> String
renderPropertyDecoder parentName name type_ =
    case type_ of
        String_ default enum ->
            "string"

        Int_ default ->
            "int"

        Float_ default ->
            "float"

        Bool_ default ->
            "bool"

        Ref_ ref ->
            decoderName ref

        Object_ props ->
            nestedDecoderName parentName name

        Array_ items ->
            nestedDecoderName parentName name
