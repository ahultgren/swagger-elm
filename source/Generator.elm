module Generator exposing (..)

import String
import Dict
import Regex exposing (regex)
import Json.Decode exposing (decodeString)
import Swagger exposing (decodeSwagger)
import Type


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map Type.renderTypes
