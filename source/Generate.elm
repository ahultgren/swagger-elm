module Generate exposing (main, generate)

import String
import Dict
import Html exposing (text)
import Html.App exposing (programWithFlags)
import Json.Decode exposing (decodeString)
import Swagger.Decode as Swagger exposing (Swagger, decodeSwagger)
import Swagger.Parse exposing (parseDefinitions)
import Generate.Type as Type
import Generate.Decoder as Decoder
import Generate.Headers as Headers


main : Program String
main =
    programWithFlags
        { init = init
        , view = text << toOutput << generate
        , update = always << always ( "", Cmd.none )
        , subscriptions = always Sub.none
        }


init : String -> ( String, Cmd x )
init json =
    ( json, Cmd.none )


toOutput : Result String String -> String
toOutput result =
    case result of
        Err err ->
            Debug.log "error" err

        Ok ok ->
            ok


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
