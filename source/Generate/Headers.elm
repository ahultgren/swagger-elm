module Generate.Headers exposing (..)


renderHeaders : String
renderHeaders =
    """module Decoder exposing (..)

import Json.Decode exposing (Decoder, string, int, float, dict, list, bool, map, customDecoder, value, decodeValue, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe name decoder =
    optional name (map Just decoder) Nothing


lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
    customDecoder value
        (\\js -> decodeValue (thunk ()) js)


"""
