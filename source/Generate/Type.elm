module Generate.Type exposing (..)

import String
import Dict
import Swagger.Parse as Parse exposing (Definitions, Definition, Definition(Definition), Enum(Enum, NotEnum), Type(Object', Array', Ref', Int', Float', String', Bool'))
import Codegen.Type exposing (typeAlias, unionType, record, recordField, list, maybe)


renderTypes : Definitions -> String
renderTypes definitions =
    List.map renderRootType definitions
        |> List.append (List.filterMap renderEnum (findEnums definitions))
        |> String.concat


renderRootType : Definition -> String
renderRootType (Definition name isRequired type') =
    typeAlias name <| renderType isRequired type'


renderType : Bool -> Type -> String
renderType isRequired type' =
    maybeWrap isRequired (renderFieldType type')


renderFieldType : Type -> String
renderFieldType type' =
    case type' of
        String' enum ->
            case enum of
                Enum name _ ->
                    name

                NotEnum ->
                    "String"

        Int' ->
            "Int"

        Float' ->
            "Float"

        Bool' ->
            "Bool"

        Object' properties ->
            record <| List.map renderProperty properties

        Array' (Definition name isRequired type') ->
            list <| renderFieldType type'

        Ref' ref ->
            ref


renderProperty : Definition -> String
renderProperty (Definition name isRequired type') =
    recordField name <| renderType isRequired type'


maybeWrap : Bool -> String -> String
maybeWrap isRequired body =
    if isRequired then
        body
    else
        maybe body


findEnums : Definitions -> Definitions
findEnums definitions =
    definitions
        |> List.concatMap
            (\(Definition name isRequired type') ->
                case type' of
                    Object' properties ->
                        List.map Just <| findEnums properties

                    String' enum ->
                        [ Just (Definition name isRequired (String' enum)) ]

                    _ ->
                        []
            )
        |> List.filterMap identity


renderEnum : Definition -> Maybe String
renderEnum (Definition _ isRequired type') =
    case type' of
        String' (Enum name enum) ->
            Just <| unionType name (List.map (enumTagName name) enum)

        _ ->
            Nothing


enumTagName : String -> String -> String
enumTagName typeName tagName =
    typeName ++ tagName
