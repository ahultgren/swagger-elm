module Generate.Type exposing (..)

import String
import Dict
import Swagger.Parse as Parse exposing (Definitions, Definition, Definition(Definition), Type(Object', Array', Ref', Int', Float', String', Bool'))
import Codegen.Type exposing (typeAlias, record, recordField, list, maybe)


renderTypes : Definitions -> String
renderTypes definitions =
    String.concat <| List.map renderRootType definitions


renderRootType : Definition -> String
renderRootType (Definition name isRequired type') =
    typeAlias name <| renderType isRequired type'


renderType : Bool -> Type -> String
renderType isRequired type' =
    maybeWrap isRequired (renderFieldType type')


renderFieldType : Type -> String
renderFieldType type' =
    case type' of
        String' ->
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
