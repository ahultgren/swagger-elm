module Swagger.Decode exposing (..)

import Json.Decode as Json exposing (Decoder, string, int, float, bool, keyValuePairs, field, list, map, value, Value, decodeValue, oneOf, lazy, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Regex exposing (regex)
import Swagger.Swagger exposing (Swagger)
import Swagger.Definition exposing (Definitions, definitions, definition, Definition)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, Dict_, String_, Enum_, Int_, Float_, Bool_, Ref_)
        , Ref
        , Properties(Properties)
        , Items(Items)
        , Property(Required, Optional, Default)
        , getDefault
        )


type alias Name =
    String


decodeSwagger : Decoder Swagger
decodeSwagger =
    decode Swagger
        |> required "definitions" decodeTypes


decodeTypes : Decoder Definitions
decodeTypes =
    keyValuePairs decodeType
        |> map (List.map (\( name, type_ ) -> definition Nothing name type_))
        |> map definitions


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
                    decodePrimitive Float_

                "boolean" ->
                    decodePrimitive Bool_

                "array" ->
                    decodeArray

                _ ->
                    decodeObject


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
        |> map (apply2 stringOrEnum)


stringOrEnum : Maybe String -> Maybe (List String) -> Type
stringOrEnum default enum =
    case enum of
        Nothing ->
            String_ default

        Just enum ->
            Enum_ default enum


decodeArray : Decoder Type
decodeArray =
    decode identity
        |> required "items" (lazy (\_ -> decodeType))
        |> map (Array_ << Items)


decodeDict : Decoder Type
decodeDict =
    decode Dict_
        |> required "additionalProperties" (lazy (\_ -> decodeType))


decodeObject : Decoder Type
decodeObject =
    decode (,,)
        |> optional "required" (list string) []
        |> maybe "properties" (lazy (\_ -> keyValuePairs decodeType))
        |> maybe "additionalProperties" (lazy (\_ -> decodeType))
        |> map objectOrDict



{-
   According to the [Swagger specification]
   (http://swagger.io/specification/#model-with-map-dictionary-properties-88)
   a dict is defined by the following json:
   {
     "type": "object",
     "additionalProperties": {
       "type": "string"
     }
   }
   So if the key "properties" is not set and the key "additionalProperties" is set
   we interpret it as a dictionary instead of an object.
-}


objectOrDict : ( List String, Maybe (List ( String, Type )), Maybe Type ) -> Type
objectOrDict ( required, properties, additionalProperties ) =
    case ( properties, additionalProperties ) of
        ( Nothing, Just addProps ) ->
            Dict_ addProps

        ( Just props, _ ) ->
            Object_ <| Properties <| List.map (property required) props

        _ ->
            Object_ <| Properties []


decodeProperties : ( List String, List ( String, Type ) ) -> List Property
decodeProperties ( required, properties ) =
    List.map (property required) properties


property : List String -> ( String, Type ) -> Property
property required ( name, type_ ) =
    case List.any ((==) name) required of
        True ->
            Required name type_

        False ->
            case getDefault type_ of
                Just default ->
                    Default name type_ default

                Nothing ->
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
