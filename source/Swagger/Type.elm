module Swagger.Type exposing (..)


type alias Default =
    Maybe String


type Type
    = Object_ Properties
    | Array_ Items
      -- TODO: remove default from each type?
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
    | Default Name Type String


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

        Default _ type_ _ ->
            type_


getPropertyName : Property -> Name
getPropertyName prop =
    case prop of
        Required name _ ->
            name

        Optional name _ ->
            name

        Default name _ _ ->
            name


getItemsType : Items -> Type
getItemsType (Items type_) =
    type_


getDefault : Type -> Maybe String
getDefault type_ =
    case type_ of
        String_ (Just default) ->
            Just default

        Int_ (Just default) ->
            Just default

        Float_ (Just default) ->
            Just default

        Bool_ (Just default) ->
            Just default

        Enum_ (Just default) _ ->
            Just default

        _ ->
            Nothing
