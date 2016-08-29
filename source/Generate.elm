module Generate exposing (..)

import String
import Dict
import Regex exposing (regex)
import Json.Decode exposing (decodeString)
import Swagger.Decode as Swagger exposing (Swagger, decodeSwagger)
import Swagger.Parse exposing (parseDefinitions)
import Generate.Type as Type
import Generate.Decoder as Decoder
import Generate.Headers as Headers


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map render


render : Swagger -> String
render swagger =
    let
        definitions =
            parseDefinitions swagger.definitions

        definitions' =
            List.map (Swagger.Parse.toNewDefinition <| Dict.keys definitions) <| Dict.toList definitions
    in
        [ Headers.renderHeaders
        , Type.renderTypes definitions'
        , Decoder.renderDecoders definitions'
        ]
            |> String.concat
