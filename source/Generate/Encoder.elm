module Generate.Encoder exposing (..)

import Generate.Utils exposing (typeName, encoderName, nestedEncoderName)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Swagger.Type exposing (getPropertyName, getPropertyType)
import Codegen.Function exposing (function, arg, pipeline, letin, caseof, lazy)
import Codegen.List exposing (list)
import Codegen.Literal exposing (string)
import Codegen.Tuple exposing (tuple)
import Codegen.Utils exposing (sanitize, uncapitalize)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, Dict_, String_, Enum_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional, Default)
        , getItemsType
        )


renderEncoder : Definition -> String
renderEncoder definition =
    let
        name =
            getFullName definition
    in
        function (encoderName <| name)
            [ arg (typeName name) (maybeUnwrapType definition name) ]
            ("Json.Encode.Value")
            (renderEncoderBody definition)


maybeUnwrapType : Definition -> String -> String
maybeUnwrapType definition name =
    case getType definition of
        Object_ _ ->
            "(" ++ typeName name ++ " value" ++ ")"

        Array_ _ ->
            "(" ++ typeName name ++ " value" ++ ")"

        Ref_ ref ->
            "(" ++ typeName name ++ " value" ++ ")"

        Dict_ _ ->
            "value"

        Enum_ _ enum ->
            "value"

        String_ _ ->
            "value"

        Int_ _ ->
            "value"

        Float_ _ ->
            "value"

        Bool_ _ ->
            "value"


renderEncoderBody : Definition -> String
renderEncoderBody definition =
    case getType definition of
        Object_ properties ->
            renderObjectBody (getFullName definition) properties

        Array_ items ->
            renderArrayBody (getFullName definition) (getItemsType items)

        Dict_ typeName ->
            renderDictBody (getFullName definition) typeName

        Enum_ _ enum ->
            renderEnumBody (getFullName definition) enum

        String_ _ ->
            renderPrimitiveBody "string"

        Int_ _ ->
            renderPrimitiveBody "int"

        Float_ _ ->
            renderPrimitiveBody "float"

        Bool_ _ ->
            renderPrimitiveBody "bool"

        Ref_ ref ->
            encoderName ref


renderPrimitiveBody : String -> String
renderPrimitiveBody typeName =
    "Json.Encode." ++ typeName ++ " value"


renderArrayBody : String -> Type -> String
renderArrayBody name type_ =
    "Json.Encode.list ("
        ++ "List.map "
        ++ (renderPropertyEncoder name "Item" type_)
        ++ " value"
        ++ ")"


renderDictBody : String -> Type -> String
renderDictBody name typeName =
    "dictEncoder "
        ++ (renderPropertyEncoder name "Property" typeName)
        ++ " value"


renderObjectBody : String -> Properties -> String
renderObjectBody name (Properties properties) =
    properties
        |> List.map (renderObjectProperty name)
        |> list
        |> (++) "Json.Encode.object "


renderObjectProperty : String -> Property -> String
renderObjectProperty parentName property =
    let
        propertyEncoder =
            renderPropertyEncoder parentName (getPropertyName property) (getPropertyType property)
    in
        case property of
            Required name type_ ->
                tuple (string name) (propertyEncoder ++ " value." ++ (uncapitalize <| sanitize name))

            Optional name type_ ->
                tuple (string name) ("Json.Encode.Extra.maybe " ++ propertyEncoder ++ " value." ++ (uncapitalize <| sanitize name))

            Default name type_ _ ->
                tuple (string name) (propertyEncoder ++ " value." ++ (uncapitalize <| sanitize name))


renderPropertyEncoder : String -> String -> Type -> String
renderPropertyEncoder parentName name type_ =
    case type_ of
        Object_ _ ->
            nestedEncoderName parentName name

        Array_ _ ->
            nestedEncoderName parentName name

        Dict_ _ ->
            nestedEncoderName parentName name

        Enum_ _ _ ->
            nestedEncoderName parentName name

        String_ _ ->
            "Json.Encode.string"

        Int_ _ ->
            "Json.Encode.int"

        Float_ _ ->
            "Json.Encode.float"

        Bool_ _ ->
            "Json.Encode.bool"

        Ref_ ref ->
            encoderName ref


renderEnumBody : String -> List String -> String
renderEnumBody parentName enum =
    caseof "value" <|
        List.map renderEnumEach enum


renderEnumEach : String -> ( String, String )
renderEnumEach value =
    ( typeName value, "Json.Encode.string " ++ string value )
