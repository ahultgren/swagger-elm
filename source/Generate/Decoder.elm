module Generate.Decoder exposing (..)

import Json.Decode as Json exposing (decodeString)
import Generate.Utils exposing (typeName, decoderName, nestedDecoderName)
import Codegen.Function as Fun exposing (function, pipeline, letin, caseof)
import Codegen.Literal exposing (string)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, Dict_, String_, Enum_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional, Default)
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

        Dict_ typeName ->
            renderDictBody (getFullName definition) typeName

        Enum_ default enum ->
            renderEnumBody (getFullName definition) enum

        String_ default ->
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


renderDictBody : String -> Type -> String
renderDictBody name type_ =
    "dict " ++ (renderPropertyDecoder name "Property" type_)


renderObjectBody : String -> Properties -> String
renderObjectBody name (Properties properties) =
    properties
        |> List.map (renderObjectDecoderProperty name)
        |> pipeline ((++) "decode " <| typeName name)


renderObjectDecoderProperty : String -> Property -> String
renderObjectDecoderProperty parentName property =
    case property of
        Required name type_ ->
            "required " ++ string name ++ " " ++ renderPropertyDecoder parentName name type_

        Optional name type_ ->
            "maybe " ++ string name ++ " " ++ renderPropertyDecoder parentName name type_

        Default name type_ default ->
            "optional "
                ++ string name
                ++ " "
                ++ renderPropertyDecoder parentName name type_
                ++ " "
                ++ defaultValue type_ default


defaultValue : Type -> String -> String
defaultValue type_ default =
    case type_ of
        Enum_ _ _ ->
            case decodeString Json.string default of
                Ok newDefault ->
                    typeName newDefault

                Err err ->
                    Debug.crash "Invalid default value" err default

        _ ->
            default


renderPropertyDecoder : String -> String -> Type -> String
renderPropertyDecoder parentName name type_ =
    case type_ of
        Object_ props ->
            nestedDecoderName parentName name

        Array_ items ->
            nestedDecoderName parentName name

        Dict_ _ ->
            nestedDecoderName parentName name

        Enum_ _ _ ->
            nestedDecoderName parentName name

        String_ default ->
            "string"

        Int_ default ->
            "int"

        Float_ default ->
            "float"

        Bool_ default ->
            "bool"

        Ref_ ref ->
            decoderName ref


renderEnumBody : String -> List String -> String
renderEnumBody parentName enum =
    let
        decoderName_ =
            decoderName parentName
    in
        (letin
            [ ( "decodeToType string"
              , caseof "string"
                    ((List.map renderEnumEach enum) ++ [ renderEnumFail parentName ])
              )
            ]
            "customDecoder string decodeToType"
        )


renderEnumEach : String -> ( String, String )
renderEnumEach value =
    ( string value, "Result.Ok " ++ typeName value )


renderEnumFail : String -> ( String, String )
renderEnumFail parentName =
    ( "_", "Result.Err (\"Invalid value for " ++ typeName parentName ++ ". Value: \" ++ string)" )
