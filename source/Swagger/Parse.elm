module Swagger.Parse exposing (..)

import Dict
import String
import Swagger.Decode as Decode
import Swagger.Flatten exposing (flattenNestedDefinitions, extractProperty)


type alias Definitions =
    List Definition


type Definition
    = Object' Name IsRequired Properties
    | Array' Name IsRequired Properties
    | Int' Name IsRequired
    | Float' Name IsRequired
    | String' Name IsRequired
    | Bool' Name IsRequired
    | Ref' Name IsRequired Name


type alias Name =
    String


type alias IsRequired =
    Bool


type alias Properties =
    Definitions


type alias Parser =
    ( String, Decode.Definition ) -> Definition


parseDefinitions : Decode.Definitions -> Decode.Definitions
parseDefinitions definitions =
    flattenNestedDefinitions definitions


toNewDefinition : List String -> ( String, Decode.Definition ) -> Definition
toNewDefinition parentRequired ( name, { type', ref', items, properties, required } ) =
    let
        isRequired' =
            isRequired parentRequired name
    in
        case ( type', ref' ) of
            ( _, Just ref' ) ->
                Ref' name isRequired' ref'

            ( Just type', Nothing ) ->
                case type' of
                    "string" ->
                        String' name isRequired'

                    "integer" ->
                        Int' name isRequired'

                    "number" ->
                        Float' name isRequired'

                    "boolean" ->
                        Bool' name isRequired'

                    "array" ->
                        Array' name isRequired' <| toNewItems (toNewDefinition required) items

                    "object" ->
                        Object' name isRequired' <| toNewProperties (toNewDefinition required) properties

                    _ ->
                        Object' name isRequired' <| toNewProperties (toNewDefinition required) properties

            _ ->
                Object' name isRequired' <| toNewProperties (toNewDefinition required) properties


toNewProperties : Parser -> Maybe Decode.Properties -> Definitions
toNewProperties toNewDefinition' properties =
    case properties of
        Nothing ->
            []

        Just properties ->
            Dict.toList properties
                |> List.map (toNewDefinition' << extractProperty)


toNewItems : Parser -> Maybe Decode.Property -> Definitions
toNewItems toNewDefinition' items =
    case items of
        Nothing ->
            []

        Just items ->
            [ toNewDefinition' <| extractProperty ( "que?", items ) ]


isRequired : List String -> String -> Bool
isRequired required name =
    List.member name required
