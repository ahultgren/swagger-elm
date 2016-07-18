module Decoder exposing (..)

import Dict exposing (Dict)
import Json.Encode
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))


type alias Swagger =
    { definitions : Definitions
    }


type alias Definitions =
    Dict String Definition


type alias Definition =
    { type' : Maybe String
    , required : Maybe (List String)
    , properties : Maybe Properties
    , items : Maybe Property
    , ref' : Maybe String
    }


type alias Properties =
    Dict String Property


type Property
    = Property Definition


decodeSwagger : Json.Decode.Decoder Swagger
decodeSwagger =
    Json.Decode.succeed Swagger
        |: ("definitions" := decodeDefinitions)


decodeDefinitions : Json.Decode.Decoder Definitions
decodeDefinitions =
    Json.Decode.dict decodeDefinition


decodeDefinition : Json.Decode.Decoder Definition
decodeDefinition =
    lazy
        (\_ ->
            Json.Decode.succeed Definition
                |: Json.Decode.maybe ("type" := Json.Decode.string)
                |: Json.Decode.maybe ("required" := Json.Decode.list Json.Decode.string)
                |: Json.Decode.maybe ("properties" := decodeProperties)
                |: Json.Decode.maybe ("items" := decodeDefinition |> Json.Decode.map Property)
                |: Json.Decode.maybe ("$ref" := Json.Decode.string)
        )


decodeProperties : Json.Decode.Decoder Properties
decodeProperties =
    Json.Decode.dict decodeProperty


decodeProperty : Json.Decode.Decoder Property
decodeProperty =
    lazy (\_ -> decodeDefinition) |> Json.Decode.map Property



-- helpers


lazy : (() -> Json.Decode.Decoder a) -> Json.Decode.Decoder a
lazy thunk =
    Json.Decode.customDecoder Json.Decode.value
        (\js -> Json.Decode.decodeValue (thunk ()) js)
