module Generate.Type exposing (..)

import Generate.Utils exposing (typeName, nestedTypeName)
import Codegen.Type exposing (typeAlias, unionType, record, recordField, list, maybe)
import Codegen.Utils exposing (capitalize, sanitize)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, String_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional)
        , getItemsType
        )


renderType : Definition -> String
renderType definition =
    typeAlias (sanitize <| getFullName definition) <|
        renderTypeBody (getFullName definition) <|
            getType definition


renderTypeBody : String -> Type -> String
renderTypeBody name type_ =
    case type_ of
        String_ _ enum ->
            "String"

        Int_ _ ->
            "Int"

        Float_ _ ->
            "Float"

        Bool_ _ ->
            "Bool"

        Object_ props ->
            renderRecord name props

        Array_ items ->
            list <| renderPropertyType name "Item" <| getItemsType items

        Ref_ ref ->
            typeName ref


renderRecord : String -> Properties -> String
renderRecord parentName (Properties properties) =
    record <| List.map (renderProperty parentName) properties


renderProperty : String -> Property -> String
renderProperty parentName prop =
    case prop of
        Required name type_ ->
            recordField name <| renderPropertyType parentName name type_

        Optional name type_ ->
            recordField name <| maybe <| renderPropertyType parentName name type_


renderPropertyType : String -> String -> Type -> String
renderPropertyType parentName name type_ =
    case type_ of
        String_ _ _ ->
            "String"

        Int_ _ ->
            "Int"

        Float_ _ ->
            "Float"

        Bool_ _ ->
            "Bool"

        Ref_ ref ->
            typeName ref

        Object_ _ ->
            (nestedTypeName parentName name)

        Array_ _ ->
            (nestedTypeName parentName name)
