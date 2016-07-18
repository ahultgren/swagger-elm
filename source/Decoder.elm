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
    { required : Maybe (List String)
    , properties : Maybe Properties
    }


type alias Properties =
    Dict String Property


type alias Property =
    { type' : Maybe String
    , ref' : Maybe String
    }


decodeSwagger : Json.Decode.Decoder Swagger
decodeSwagger =
    Json.Decode.succeed Swagger
        |: ("definitions" := decodeDefinitions)


decodeDefinitions : Json.Decode.Decoder Definitions
decodeDefinitions =
    Json.Decode.dict decodeDefinition


decodeDefinition : Json.Decode.Decoder Definition
decodeDefinition =
    Json.Decode.succeed Definition
        |: Json.Decode.maybe ("required" := Json.Decode.list Json.Decode.string)
        |: Json.Decode.maybe ("properties" := decodeProperties)


decodeProperties : Json.Decode.Decoder Properties
decodeProperties =
    Json.Decode.dict decodeProperty


decodeProperty : Json.Decode.Decoder Property
decodeProperty =
    Json.Decode.succeed Property
        |: Json.Decode.maybe ("type" := Json.Decode.string)
        |: Json.Decode.maybe ("$ref" := Json.Decode.string)
