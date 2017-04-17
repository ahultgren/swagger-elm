module Swagger.Flatten exposing (flatten)

import Swagger.Swagger exposing (Swagger)
import Swagger.Definition as Definition
    exposing
        ( Definitions
        , Definition
        , definition
        , singleton
        , getType
        , getName
        )
import Swagger.Type
    exposing
        ( Type(Object_, Array_, String_, Enum_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional)
        , Items(Items)
        , getPropertyType
        , getPropertyName
        )


flatten : Swagger -> Swagger
flatten ({ definitions } as swagger) =
    { swagger
        | definitions = flattenDefinitions definitions
    }


flattenDefinitions : Definitions -> Definitions
flattenDefinitions =
    Definition.foldl flattenEachRoot <| singleton


flattenEachRoot : Definition -> Definitions -> Definitions
flattenEachRoot definition definitions =
    let
        name =
            getName definition

        newDefinitions =
            case getType definition of
                Object_ props ->
                    flattenProperties [ name ] props definitions

                Array_ items ->
                    flattenItems [ name ] items definitions

                _ ->
                    definitions
    in
        Definition.prepend definition newDefinitions


flattenProperties : List String -> Properties -> Definitions -> Definitions
flattenProperties parentNames (Properties props) definitions =
    List.foldl (flattenProperty parentNames) definitions props


flattenProperty : List String -> Property -> Definitions -> Definitions
flattenProperty parentNames prop definitions =
    flattenType parentNames (getPropertyName prop) (getPropertyType prop) definitions


flattenItems : List String -> Items -> Definitions -> Definitions
flattenItems parentNames (Items type_) definitions =
    flattenType parentNames "Item" type_ definitions


flattenType : List String -> String -> Type -> Definitions -> Definitions
flattenType parentNames name type_ definitions =
    let
        childParentNames =
            name :: parentNames

        prependSelf =
            Definition.prepend
                (definition (Just parentNames) name type_)
    in
        case type_ of
            Object_ props ->
                flattenProperties childParentNames props definitions
                    |> prependSelf

            Array_ items ->
                flattenItems childParentNames items definitions
                    |> prependSelf

            (Enum_ _ _) as enum ->
                definitions
                    |> prependSelf

            _ ->
                definitions
