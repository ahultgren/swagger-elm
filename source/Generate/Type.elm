module Generate.Type exposing (..)

import Generate.Utils exposing (typeName, nestedTypeName)
import Codegen.Utils exposing (sanitize)
import Codegen.Type exposing (typeAlias, unionType, record, recordField, list, dict, maybe)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Swagger.Type
    exposing
        ( Type(Object_, Dict_, Array_, String_, Enum_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional, Default)
        , getItemsType
        )


renderType : Definition -> String
renderType definition =
    let
        name =
            typeName <| getFullName definition

        type_ =
            getType definition

        typeAliasDecl =
            typeAlias name

        unionTypeDecl =
            unionType name
    in
        case type_ of
            String_ _ ->
                typeAliasDecl "String"

            Int_ _ ->
                typeAliasDecl "Int"

            Float_ _ ->
                typeAliasDecl "Float"

            Bool_ _ ->
                typeAliasDecl "Bool"

            Enum_ _ enum ->
                unionTypeDecl <| renderEnum name enum

            Object_ props ->
                typeAliasDecl <| renderRecord name props

            Dict_ items ->
                typeAliasDecl <| dict "String" <| renderPropertyType name "Property" <| getItemsType items

            Array_ items ->
                typeAliasDecl <| list <| renderPropertyType name "Item" <| getItemsType items

            Ref_ ref ->
                typeAliasDecl <| typeName ref


renderEnum : String -> List String -> List String
renderEnum name =
    List.map typeName


renderRecord : String -> Properties -> String
renderRecord parentName (Properties properties) =
    record <| List.map (renderProperty parentName) properties


renderProperty : String -> Property -> String
renderProperty parentName prop =
    case prop of
        Required name type_ ->
            recordField (sanitize name) <| renderPropertyType parentName name type_

        Optional name type_ ->
            recordField (sanitize name) <| maybe <| renderPropertyType parentName name type_

        Default name type_ _ ->
            recordField (sanitize name) <| renderPropertyType parentName name type_


renderPropertyType : String -> String -> Type -> String
renderPropertyType parentName name type_ =
    case type_ of
        Object_ _ ->
            (nestedTypeName parentName name)

        Dict_ _ ->
            (nestedTypeName parentName name)

        Array_ _ ->
            (nestedTypeName parentName name)

        Enum_ _ _ ->
            (nestedTypeName parentName name)

        String_ _ ->
            "String"

        Int_ _ ->
            "Int"

        Float_ _ ->
            "Float"

        Bool_ _ ->
            "Bool"

        Ref_ ref ->
            typeName ref
