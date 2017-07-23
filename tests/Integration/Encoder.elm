module Integration.Encoder exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Dict
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Integration.Decoder exposing (expectedArticle, expectedErrorResponse, expectedGroup, expectedRules, expectedLabels, expectedDictWithObject, expectedDictWithRef, expectedComment)
import Swagger exposing (decodeArticle, encodeArticle, decodeErrorResponse, decodeGroup, decodeRules, decodeLabels, decodeDictWithObject, decodeDictWithRef, decodeComment, encodeErrorResponse, encodeGroup, encodeRules, encodeLabels, encodeDictWithObject, encodeDictWithRef, encodeComment)


all : Test
all =
    describe "Encoder"
        [ test "should encode Article" <|
            always <|
                Expect.equal (Ok expectedArticle) (decodeString decodeArticle (encode 0 (encodeArticle expectedArticle)))
        , test "should encode ErrorResponse" <|
            always <|
                Expect.equal (Ok expectedErrorResponse) (decodeString decodeErrorResponse (encode 0 (encodeErrorResponse expectedErrorResponse)))
        , test "should encode Group" <|
            always <|
                Expect.equal (Ok expectedGroup) (decodeString decodeGroup (encode 0 (encodeGroup expectedGroup)))
        , test "should encode Rules" <|
            always <|
                Expect.equal (Ok expectedRules) (decodeString decodeRules (encode 0 (encodeRules expectedRules)))
        , test "should encode labels" <|
            always <|
                Expect.equal (Ok expectedLabels) (decodeString decodeLabels (encode 0 (encodeLabels expectedLabels)))
        , test "should encode empty dict" <|
            always <|
                Expect.equal (Ok <| Dict.fromList []) (decodeString decodeLabels (encode 0 (encodeLabels <| Dict.fromList [])))
        , test "should encode dict with nested object" <|
            always <|
                Expect.equal (Ok expectedDictWithObject) (decodeString decodeDictWithObject (encode 0 (encodeDictWithObject expectedDictWithObject)))
        , test "should encode dict with nested ref" <|
            always <|
                Expect.equal (Ok expectedDictWithRef) (decodeString decodeDictWithRef (encode 0 (encodeDictWithRef expectedDictWithRef)))
        , test "should encode comment (recursive)" <|
            always <|
                Expect.equal (Ok expectedComment) (decodeString decodeComment (encode 0 (encodeComment expectedComment)))
        ]
