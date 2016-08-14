module Generate exposing (..)

import String
import Dict
import Regex exposing (regex)
import Json.Decode exposing (decodeString)
import Swagger.Decode as Swagger exposing (Swagger, decodeSwagger)
import Generate.Type as Type
import Generate.Decoder as Decoder


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map render


render : Swagger -> String
render swagger =
    applyList [ Type.renderTypes, Decoder.renderDecoders ] swagger
        |> String.concat


applyList : List (a -> b) -> a -> List b
applyList fns value =
    List.map (apply value) fns


apply : a -> (a -> b) -> b
apply value fn =
    fn value
