module Swagger.Decode exposing (..)

import Json.Decode as Json exposing (Decoder, string, int, float, bool, keyValuePairs, list, map, value, decodeValue, oneOf, lazy, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Regex exposing (regex)


type alias Swagger =
    { definitions : Definitions
    }


type Definitions
    = Definitions (List Definition)


type Definition
    = Definition Name Type


type Type
    = Object_ Properties
    | Array_ Items
    | String_ Default Enum
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


type alias Default =
    Maybe String


type alias Enum =
    Maybe (List String)


decodeSwagger : Decoder Swagger
decodeSwagger =
    decode Swagger
        |> required "definitions" decodeTypes


decodeTypes : Decoder Definitions
decodeTypes =
    keyValuePairs decodeType
        |> map (List.map (\( name, type_ ) -> Definition name type_))
        |> map Definitions


decodeType : Decoder Type
decodeType =
    lazy
        (\_ ->
            decode (,)
                |> optional "type" string ""
                |> maybe "$ref" string
                |> andThen decodeTypeByType
        )


decodeTypeByType : ( String, Maybe String ) -> Decoder Type
decodeTypeByType ( type_, ref ) =
    case ref of
        Just ref_ ->
            decodeRef

        Nothing ->
            case type_ of
                "string" ->
                    decodeString

                "integer" ->
                    decodePrimitive Int_

                "number" ->
                    decodePrimitive Int_

                "bool" ->
                    decodePrimitive Int_

                "array" ->
                    decodeArray

                _ ->
                    lazy (\_ -> decodeObject)


decodeRef : Decoder Type
decodeRef =
    decode identity
        |> required "$ref" string
        |> map (Ref_ << extractRef)


extractRef : String -> Ref
extractRef ref =
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


decodePrimitive : (Maybe String -> Type) -> Decoder Type
decodePrimitive constructor =
    decode identity
        |> maybe "default" decodeAlwaysString
        |> map constructor


decodeString : Decoder Type
decodeString =
    decode (,)
        |> maybe "default" decodeAlwaysString
        |> maybe "enum" (list string)
        |> map (apply2 String_)


decodeArray : Decoder Type
decodeArray =
    decode identity
        |> required "items" (lazy (\_ -> decodeType))
        |> map (Array_ << Items)


decodeObject : Decoder Type
decodeObject =
    decode (,)
        |> optional "required" (list string) []
        |> optional "properties" (lazy (\_ -> keyValuePairs decodeType)) []
        |> map decodeProperties
        |> map (Object_ << Properties)


decodeProperties : ( List String, List ( String, Type ) ) -> List Property
decodeProperties ( required, properties ) =
    List.map (property required) properties


property : List String -> ( String, Type ) -> Property
property required ( name, type_ ) =
    case List.any ((==) name) required of
        True ->
            Required name type_

        False ->
            Optional name type_



-- helpers


apply2 : (a -> b -> c) -> ( a, b ) -> c
apply2 fn ( a, b ) =
    fn a b


decodeAlwaysString : Decoder String
decodeAlwaysString =
    oneOf
        [ string |> map toString
        , int |> map toString
        , float |> map toString
        , bool |> map toString
        ]


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe name decoder =
    optional name (map Just decoder) Nothing
