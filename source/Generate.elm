module Generate exposing (..)

import String
import Dict
import Regex exposing (regex)
import Json.Decode exposing (decodeString)
import Swagger.Decode as Swagger exposing (Swagger, decodeSwagger)
import Swagger.Parse exposing (parseDefinitions)
import Generate.Type as Type
import Generate.Decoder as Decoder


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map render


render : Swagger -> String
render swagger =
    let
        definitions =
            parseDefinitions swagger.definitions

        definitionsTempWhileRefactoring =
            List.map (Swagger.Parse.toNewDefinition <| Dict.keys definitions) <| Dict.toList definitions

        swagger' =
            { swagger | definitions = definitions }
    in
        [ Type.renderTypes definitionsTempWhileRefactoring, Decoder.renderDecoders swagger' ]
            |> String.concat


applyList : List (a -> b) -> a -> List b
applyList fns value =
    List.map ((|>) value) fns
