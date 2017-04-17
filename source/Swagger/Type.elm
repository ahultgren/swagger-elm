module Swagger.Type exposing (..)


type alias Default =
    Maybe String


type Type
    = Object_ Properties
    | Array_ Items
    | String_ Default
    | Enum_ Default Enum
    | Int_ Default
    | Float_ Default
    | Bool_ Default
    | Ref_ Ref


type Properties
    = Properties (List Property)


type Property
    = Required Name Type
    | Optional Name Type


type Items
    = Items Type


type alias Name =
    String


type alias Ref =
    String


type alias Enum =
    List String


getPropertyType : Property -> Type
getPropertyType prop =
    case prop of
        Required _ type_ ->
            type_

        Optional _ type_ ->
            type_


getPropertyName : Property -> Name
getPropertyName prop =
    case prop of
        Required name _ ->
            name

        Optional name _ ->
            name


getItemsType : Items -> Type
getItemsType (Items type_) =
    type_
