module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Json.Decode exposing (decodeString)
import Decoder exposing (Article, decodeArticle, decodeErrorResponse, Enum'displaySize(Enum'displaySizelarge, Enum'displaySizesmall))


articleJson =
    """
{
  "id": "article1",
  "title": "Article one",
  "category_id": "blog",
  "displaySize": "large",
  "nested": {
    "one": "1",
    "grandChildArray": [
      {
        "grandAProp": "Array child one"
      },
      {
        "grandAProp": "Array child two"
      }
    ],
    "grandChildObject": {
      "grandOProp": "not a float"
    }
  }
}
"""


expectedArticle =
    { id = "article1"
    , title = "Article one"
    , category_id = "blog"
    , displaySize = Enum'displaySizelarge
    , nested =
        Just
            { one = Just "1"
            , two = Nothing
            , grandChildArray =
                Just
                    ([ { grandAProp = Just "Array child one" }
                     , { grandAProp = Just "Array child two" }
                     ]
                    )
            , grandChildObject = Just { grandOProp = Just "not a float" }
            }
    , rules = Nothing
    , sponsored = False
    }


errorResponseJson =
    """
{
  "message": "Everything went wrong",
  "level": 9000.1
}
"""


expectedErrorResponse =
    { message = "Everything went wrong"
    , code = 0
    , level = 9000.1
    , readableCode = "fail"
    }


all : Test
all =
    describe "Decoder"
        [ test "should decode article" <|
            always <|
                Expect.equal (Ok expectedArticle) (decodeString decodeArticle articleJson)
        , test "should decode error response" <|
            always <|
                Expect.equal (Ok expectedErrorResponse) (decodeString decodeErrorResponse errorResponseJson)
        ]
