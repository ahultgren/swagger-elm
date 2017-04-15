module Swagger.Definition
    exposing
        ( Definitions
        , Definition
        , Names
        , Name
        , singleton
        , definitions
        , definition
        , getType
        , getName
        , getFullName
        , map
        , prepend
        , foldl
        )

import Swagger.Type exposing (Type)
import Codegen.Utils exposing (capitalize)


type Definitions
    = Definitions (List Definition)


type Definition
    = Definition Name Type
    | NestedDefinition Names Name Type


type alias Names =
    List Name


type alias Name =
    String


singleton : Definitions
singleton =
    Definitions []


definitions : List Definition -> Definitions
definitions defs =
    Definitions defs


definition : Maybe (List String) -> String -> Type -> Definition
definition parentNames name type_ =
    case parentNames of
        Nothing ->
            Definition name type_

        Just parents ->
            NestedDefinition parents name type_


getType : Definition -> Type
getType definition =
    case definition of
        Definition _ type_ ->
            type_

        NestedDefinition _ _ type_ ->
            type_


getName : Definition -> Name
getName definition =
    case definition of
        Definition name _ ->
            name

        NestedDefinition _ name _ ->
            name


getFullName : Definition -> Name
getFullName definition =
    case definition of
        Definition name _ ->
            name

        NestedDefinition parentNames name _ ->
            parentNames
                |> List.reverse
                |> List.map capitalize
                |> String.concat
                |> (flip (++) <| capitalize name)


map : (Definition -> a) -> Definitions -> List a
map fn (Definitions list) =
    List.map fn list


prepend : Definition -> Definitions -> Definitions
prepend def (Definitions defs) =
    Definitions (def :: defs)


foldl : (Definition -> Definitions -> Definitions) -> Definitions -> Definitions -> Definitions
foldl fn init (Definitions defs) =
    List.foldl fn init defs
