module Decoder exposing (..)

import Dict exposing (Dict)
import Json.Encode
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))


type alias Swagger =
    { definitions : SwaggerDefinitions
    }


type alias SwaggerDefinitions =
    Dict String SwaggerDefinition


type alias SwaggerDefinition =
    { required : Maybe (List String)
    , properties : Maybe SwaggerDefinitionProperties
    }


type alias SwaggerDefinitionProperties =
    Dict String SwaggerDefinitionProperty


type alias SwaggerDefinitionProperty =
    { type' : Maybe String
    , ref' : Maybe String
    }


decodeSwagger : Json.Decode.Decoder Swagger
decodeSwagger =
    Json.Decode.succeed Swagger
        |: ("definitions" := decodeSwaggerDefinitions)


decodeSwaggerDefinitions : Json.Decode.Decoder SwaggerDefinitions
decodeSwaggerDefinitions =
    Json.Decode.dict decodeSwaggerDefinition


decodeSwaggerDefinition : Json.Decode.Decoder SwaggerDefinition
decodeSwaggerDefinition =
    Json.Decode.succeed SwaggerDefinition
        |: Json.Decode.maybe ("required" := Json.Decode.list Json.Decode.string)
        |: Json.Decode.maybe ("properties" := decodeSwaggerDefinitionProperties)


decodeSwaggerDefinitionProperties : Json.Decode.Decoder SwaggerDefinitionProperties
decodeSwaggerDefinitionProperties =
    Json.Decode.dict decodeSwaggerDefinitionProperty


decodeSwaggerDefinitionProperty : Json.Decode.Decoder SwaggerDefinitionProperty
decodeSwaggerDefinitionProperty =
    Json.Decode.succeed SwaggerDefinitionProperty
        |: Json.Decode.maybe ("type" := Json.Decode.string)
        |: Json.Decode.maybe ("$ref" := Json.Decode.string)
