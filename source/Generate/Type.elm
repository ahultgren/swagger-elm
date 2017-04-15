module Generate.Type exposing (..)

import Codegen.Type exposing (typeAlias, unionType, record, recordField, list, maybe)
import Codegen.Utils exposing (capitalize, sanitize)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, String_, Int_, Float_, Bool_, Ref_)
        , Properties
        )


renderType : Definition -> String
renderType definition =
    typeAlias (sanitize <| getFullName definition) <| renderTypeBody <| getType definition


renderTypeBody : Type -> String
renderTypeBody type_ =
    case type_ of
        String_ _ enum ->
            "String"

        Int_ _ ->
            "Int"

        Float_ _ ->
            "Float"

        Bool_ _ ->
            "Bool"

        Object_ _ ->
            "todo"

        Array_ _ ->
            "todo"

        Ref_ ref ->
            (capitalize <| sanitize ref)
