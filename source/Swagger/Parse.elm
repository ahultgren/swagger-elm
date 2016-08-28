module Swagger.Parse exposing (..)

import Dict
import String
import Swagger.Decode as Decode
import Swagger.Flatten exposing (flattenNestedDefinitions, extractProperty)


type alias Definitions =
    List Definition


type Definition
    = Definition Name IsRequired Type


type Type
    = Object' Properties
    | Array' Definition
    | Ref' Name
    | Int'
    | Float'
    | String'
    | Bool'


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
                Definition name isRequired' (Ref' ref')

            ( Just type', Nothing ) ->
                case type' of
                    "string" ->
                        Definition name isRequired' String'

                    "integer" ->
                        Definition name isRequired' Int'

                    "number" ->
                        Definition name isRequired' Float'

                    "boolean" ->
                        Definition name isRequired' Bool'

                    "array" ->
                        Definition name isRequired' <| Array' <| toNewItems (toNewDefinition required) items

                    "object" ->
                        Definition name isRequired' <| Object' <| toNewProperties (toNewDefinition required) properties

                    _ ->
                        Definition name isRequired' <| Object' <| toNewProperties (toNewDefinition required) properties

            _ ->
                Definition name isRequired' <| Object' <| toNewProperties (toNewDefinition required) properties


toNewProperties : Parser -> Maybe Decode.Properties -> Definitions
toNewProperties toNewDefinition' properties =
    case properties of
        Nothing ->
            []

        Just properties ->
            Dict.toList properties
                |> List.map (toNewDefinition' << extractProperty)


toNewItems : Parser -> Maybe Decode.Property -> Definition
toNewItems toNewDefinition' items =
    case items of
        Nothing ->
            toNewDefinition' ( "wtf?", Decode.Definition Nothing [] Nothing Nothing Nothing )

        Just items ->
            toNewDefinition' <| extractProperty ( "que?", items )


isRequired : List String -> String -> Bool
isRequired required name =
    List.member name required
