module Generator exposing (..)

import Json.Decode exposing (decodeString)
import Decoder exposing (Swagger, decodeSwagger)


generate : String -> Result String Swagger
generate json =
    decodeString decodeSwagger json
