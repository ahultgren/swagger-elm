module Generate.Headers exposing (..)


renderHeaders : String
renderHeaders =
    """module Swagger exposing (..)

import Json.Decode exposing (Decoder, string, int, float, dict, list, bool, map, value, decodeValue, decodeString, lazy, succeed, fail, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode
import Json.Encode.Extra
import Dict exposing (Dict)


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe name decoder =
    optional name (map Just decoder) Nothing


customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder toResult =
    andThen
        (\\a ->
            case toResult a of
                Ok b ->
                    succeed b

                Err err ->
                    fail err
        )
        decoder


dictEncoder : (a -> Json.Encode.Value) -> Dict String a -> Json.Encode.Value
dictEncoder enc dict =
    Dict.toList dict
        |> List.map (\\(k,v) -> (k, enc v))
        |> Json.Encode.object

"""
