module Integration.Decoder exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Json.Decode exposing (decodeString)
import Decoder exposing (Article, decodeArticle, decodeErrorResponse, decodeGroup, decodeRules, decodeObjectDict, ArticleDisplaySize(Large, Small))
import Dict


articleJson =
    """
{
  "type": "article",
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
  "lowerCaseDefinitionRef": true,
  "lowerCaseDefinitionObjectRef": {},
  "$ref": "ref",
  "map": {
    "1": "one",
    "2": "two"
  }
}
"""


expectedArticle =
    { id = "article1"
    , type_ = Just "article"
    , title = "Article one"
    , category_id = "blog"
    , displaySize = Large
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
            , arrayOfStrings = Nothing
            }
    , rules = Nothing
    , sponsored = False
    , upperCasedField =
        { upperCasedFieldSubfield = "test"
        }
    , lowerCaseDefinitionRef = Just True
    , lowerCaseDefinitionObjectRef = Just {}
    , ref = Just "ref"
    , map = Just <| Dict.fromList [ ( "1", "one" ), ( "2", "two" ) ]
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
      , type_ = Nothing
      , title = "Article one"
      , category_id = "blog"
      , displaySize = Small
      , nested = Nothing
      , rules = Nothing
      , sponsored = False
      , upperCasedField =
            { upperCasedFieldSubfield = "test"
            }
      , lowerCaseDefinitionRef = Nothing
      , lowerCaseDefinitionObjectRef = Nothing
      , ref = Nothing
      , map = Nothing
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


dictJson =
    """
{
    "key": { "message": "this is a dict" }
}
"""


expectedDict =
    Dict.fromList
        [ ( "key"
          , { message = "this is a dict"
            , code = 0
            , readableCode = "fail"
            , level = 1.1
            }
          )
        ]


emptyDictJson =
    "{}"


expectedEmptyDict =
    Dict.empty


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
        , test "should decode ObjectDict" <|
            always <|
                Expect.equal (Ok expectedDict) (decodeString decodeObjectDict dictJson)
        , test "should decode empty dict" <|
            always <|
                Expect.equal (Ok expectedEmptyDict) (decodeString decodeObjectDict emptyDictJson)
        ]
