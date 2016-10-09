module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Json.Decode exposing (decodeString)
import Decoder exposing (Article, decodeArticle, decodeErrorResponse, decodeGroup, decodeRules, Enum'displaySize(Enum'displaySizelarge, Enum'displaySizesmall))


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
      "grandOProp": 1.1
    }
  },
  "UpperCasedField": {
    "UpperCasedFieldSubfield": "test"
  },
  "lowerCaseDefinitionRef": true
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
            , grandChildObject = Just { grandOProp = Just 1.1 }
            }
    , rules = Nothing
    , sponsored = False
    , upperCasedField =
        { upperCasedFieldSubfield = "test"
        }
    , lowerCaseDefinitionRef = Just True
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


groupJson =
    """
[{
    "id": "article1",
    "title": "Article one",
    "category_id": "blog",
    "UpperCasedField": {
      "UpperCasedFieldSubfield": "test"
    }
}]
"""


expectedGroup =
    [ { id = "article1"
      , title = "Article one"
      , category_id = "blog"
      , displaySize = Enum'displaySizesmall
      , nested = Nothing
      , rules = Nothing
      , sponsored = False
      , upperCasedField =
            { upperCasedFieldSubfield = "test"
            }
      , lowerCaseDefinitionRef = Nothing
      }
    ]


rulesJson =
    """
{
    "unknownField": "secret"
}
"""


expectedRules =
    {}


all : Test
all =
    describe "Decoder"
        [ test "should decode Article" <|
            always <|
                Expect.equal (Ok expectedArticle) (decodeString decodeArticle articleJson)
        , test "should decode ErrorResponse" <|
            always <|
                Expect.equal (Ok expectedErrorResponse) (decodeString decodeErrorResponse errorResponseJson)
        , test "should decode Group" <|
            always <|
                Expect.equal (Ok expectedGroup) (decodeString decodeGroup groupJson)
        , test "should decode Rules" <|
            always <|
                Expect.equal (Ok expectedRules) (decodeString decodeRules rulesJson)
        ]
