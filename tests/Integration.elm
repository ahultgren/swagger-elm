module Integration exposing (..)

import Html exposing (text)
import Html.App exposing (programWithFlags)
import Json.Decode exposing (decodeString)
import Decoder


main : Program String
main =
    programWithFlags
        { init = init
        , view = text << toOutput << parse
        , update = always << always ( "", Cmd.none )
        , subscriptions = always Sub.none
        }


init : String -> ( String, Cmd x )
init json =
    ( json, Cmd.none )


toOutput : Result String a -> String
toOutput result =
    case result of
        Ok a ->
            -- Spaces needed because of a system bug somewhere causing stdout
            -- not being written when run through npm
            "Ok                                                                                                                                                                                                        "

        Err err ->
            toString err


parse : String -> Result String Decoder.Article
parse =
    decodeString Decoder.decodeArticle
