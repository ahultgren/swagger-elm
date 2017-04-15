module Swagger.Flatten exposing (..)

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
        ( Type(Object_, Array_, String_, Int_, Float_, Bool_, Ref_)
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
flattenEachRoot definition newDefinitions =
    let
        name =
            getName definition

        newDefinitions_ =
            case getType definition of
                Object_ props ->
                    flattenProperties [ name ] props newDefinitions

                Array_ items ->
                    flattenItems [ name ] items newDefinitions

                _ ->
                    newDefinitions
    in
        Definition.prepend definition newDefinitions_


flattenProperties : List String -> Properties -> Definitions -> Definitions
flattenProperties parentNames (Properties props) definitions =
    List.foldl (flattenProperty parentNames) definitions props


flattenProperty : List String -> Property -> Definitions -> Definitions
flattenProperty parentNames prop definitions =
    let
        newParentNames =
            getPropertyName prop :: parentNames

        newDefinitions =
            case getPropertyType prop of
                Object_ props ->
                    Definition.prepend
                        (propToDefinition parentNames prop)
                        (flattenProperties newParentNames props definitions)

                Array_ items ->
                    Definition.prepend
                        (propToDefinition parentNames prop)
                        (flattenItems newParentNames items definitions)

                _ ->
                    definitions
    in
        newDefinitions


flattenItems : List String -> Items -> Definitions -> Definitions
flattenItems parentNames (Items type_) definitions =
    let
        name =
            "Item"

        newParentNames =
            name :: parentNames

        newDefinitions =
            case type_ of
                Object_ props ->
                    Definition.prepend
                        (typeToDefinition parentNames name type_)
                        (flattenProperties newParentNames props definitions)

                Array_ items ->
                    Definition.prepend
                        (typeToDefinition parentNames name type_)
                        (flattenItems newParentNames items definitions)

                _ ->
                    definitions
    in
        newDefinitions


propToDefinition : List String -> Property -> Definition
propToDefinition parentNames prop =
    typeToDefinition parentNames (getPropertyName prop) (getPropertyType prop)


typeToDefinition : List String -> String -> Type -> Definition
typeToDefinition parentNames name type_ =
    definition (Just parentNames) name type_
