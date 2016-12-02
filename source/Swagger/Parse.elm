module Swagger.Parse exposing (..)

import Dict
import Regex exposing (regex)
import Swagger.Decode as Decode
import Swagger.Flatten exposing (flattenNestedDefinitions, extractProperty)
import Codegen.Utils exposing (capitalize)


type alias Definitions =
    List Definition


type Definition
    = Definition Name IsRequired Type


type Type
    = Object_ Properties
    | Array_ Definition
    | Ref_ Name
    | String_ Enum
    | Int_
    | Float_
    | Bool_


type alias Name =
    String


type IsRequired
    = IsRequired
    | NotRequired (Maybe String)


type alias Properties =
    Definitions


type Enum
    = Enum Name (List String)
    | NotEnum


type alias Parser =
    ( String, Decode.Definition ) -> Definition


parseDefinitions : Decode.Definitions -> Decode.Definitions
parseDefinitions definitions =
    flattenNestedDefinitions definitions


toNewDefinition : List String -> ( String, Decode.Definition ) -> Definition
toNewDefinition parentRequired ( name, { type_, ref_, items, properties, required, enum, default } ) =
    let
        isRequired_ =
            isRequired parentRequired name default
    in
        case ( type_, ref_ ) of
            ( _, Just ref_ ) ->
                Definition name isRequired_ (Ref_ <| extractRefType ref_)

            ( Just type_, Nothing ) ->
                case type_ of
                    "string" ->
                        Definition name isRequired_ <| String_ (makeEnum name enum)

                    "integer" ->
                        Definition name isRequired_ Int_

                    "number" ->
                        Definition name isRequired_ Float_

                    "boolean" ->
                        Definition name isRequired_ Bool_

                    "array" ->
                        Definition name isRequired_ <| Array_ <| toNewItems (toNewDefinition required) items

                    "object" ->
                        Definition name isRequired_ <| Object_ <| toNewProperties (toNewDefinition required) properties

                    _ ->
                        Definition name isRequired_ <| Object_ <| toNewProperties (toNewDefinition required) properties

            _ ->
                Definition name isRequired_ <| Object_ <| toNewProperties (toNewDefinition required) properties


toNewProperties : Parser -> Maybe Decode.Properties -> Definitions
toNewProperties toNewDefinition_ properties =
    case properties of
        Nothing ->
            []

        Just properties ->
            Dict.toList properties
                |> List.map (toNewDefinition_ << extractProperty)


toNewItems : Parser -> Maybe Decode.Property -> Definition
toNewItems toNewDefinition_ items =
    case items of
        Nothing ->
            toNewDefinition_ ( "TODO WTF", Decode.Definition Nothing [] Nothing Nothing Nothing Nothing Nothing )

        Just items ->
            toNewDefinition_ <| extractProperty ( "TODO FIX", items )


isRequired : List String -> String -> Maybe String -> IsRequired
isRequired required name default =
    if List.member name required then
        IsRequired
    else
        NotRequired default


extractRefType : String -> String
extractRefType ref =
    let
        parsed =
            (List.head (Regex.find (Regex.AtMost 1) (regex "^#/definitions/(.+)$") ref))
                |> Maybe.andThen (List.head << .submatches)
    in
        case parsed of
            Just (Just ref_) ->
                ref_

            _ ->
                Debug.crash "Unparseable reference " ++ ref


makeEnum : Name -> Maybe (List String) -> Enum
makeEnum name enum =
    case enum of
        Just enum ->
            Enum (enumName name) enum

        Nothing ->
            NotEnum


enumName : String -> String
enumName name =
    "Enum" ++ capitalize name
