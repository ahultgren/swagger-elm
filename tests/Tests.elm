module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Json.Decode exposing (decodeString)
import Integration.Decoder
import Unit.Codegen.Utils


all : Test
all =
    describe "Tests"
        [ Integration.Decoder.all
        , Unit.Codegen.Utils.all
        ]
