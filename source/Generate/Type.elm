module Generate.Type exposing (..)

import String
import Swagger.Parse as Parse
    exposing
        ( Definitions
        , Definition
        , Definition(Definition)
        , Enum(Enum, NotEnum)
        , IsRequired(IsRequired, NotRequired)
        , Type(Object_, Array_, Ref_, Int_, Float_, String_, Bool_)
        )
import Codegen.Type exposing (typeAlias, unionType, record, recordField, list, maybe)
import Codegen.Utils exposing (capitalize, sanitize)


renderTypes : Definitions -> String
renderTypes definitions =
    List.map renderRootType definitions
        |> List.append (List.filterMap renderEnum (findEnums definitions))
        |> String.concat


renderRootType : Definition -> String
renderRootType (Definition name isRequired type_) =
    typeAlias (sanitize name) <| renderType isRequired type_


renderType : IsRequired -> Type -> String
renderType isRequired type_ =
    maybeWrap isRequired (renderFieldType type_)


renderFieldType : Type -> String
renderFieldType type_ =
    case type_ of
        String_ enum ->
            case enum of
                Enum name _ ->
                    sanitize name

                NotEnum ->
                    "String"

        Int_ ->
            "Int"

        Float_ ->
            "Float"

        Bool_ ->
            "Bool"

        Object_ properties ->
            record <| List.map renderProperty properties

        Array_ (Definition name isRequired type_) ->
            list <| renderFieldType type_

        Ref_ ref ->
            (capitalize <| sanitize ref)


renderProperty : Definition -> String
renderProperty (Definition name isRequired type_) =
    recordField (sanitize name) <| renderType isRequired type_


maybeWrap : IsRequired -> String -> String
maybeWrap isRequired body =
    case isRequired of
        IsRequired ->
            body

        NotRequired default ->
            case default of
                Just _ ->
                    body

                Nothing ->
                    maybe body


findEnums : Definitions -> Definitions
findEnums definitions =
    definitions
        |> List.concatMap
            (\(Definition name isRequired type_) ->
                case type_ of
                    Object_ properties ->
                        List.map Just <| findEnums properties

                    String_ enum ->
                        [ Just (Definition name isRequired (String_ enum)) ]

                    _ ->
                        []
            )
        |> List.filterMap identity


renderEnum : Definition -> Maybe String
renderEnum (Definition _ isRequired type_) =
    case type_ of
        String_ (Enum name enum) ->
            Just <| unionType (sanitize name) (List.map (sanitize << enumTagName name) enum)

        _ ->
            Nothing


enumTagName : String -> String -> String
enumTagName typeName tagName =
    typeName ++ capitalize tagName
