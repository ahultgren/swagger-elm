module Integration.Decoder exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Json.Decode exposing (decodeString)
import Decoder exposing (Article, decodeArticle, decodeErrorResponse, decodeGroup, decodeRules, decodeLabels, decodeDictWithObject, decodeDictWithRef, ArticleDisplaySize(Large, Small))
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
  "$ref": "ref"
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


labelsJson =
    """
 {
           "label1": "labelContent",
           "label2": "labelContent",
           "label3": "labelContent"
       }
     """


expectedLabels =
    Dict.fromList
        [ ( "label1", "labelContent" )
        , ( "label2", "labelContent" )
        , ( "label3", "labelContent" )
        ]

dictWithObjectJson =
    """
     {
     "label1": {"nestedProperty": "value1"},
     "label2": {"nestedProperty": "value2"},
     "label3": {"nestedProperty": "value3"},
     "label4": {"nestedProperty": "value4"},
     "label5": {"nestedProperty": "value5"}
     }
     """

expectedDictWithObject =
    Dict.fromList
        [( "label1", {nestedProperty = Just "value1"})
        ,( "label2", {nestedProperty = Just "value2"})
        ,( "label3", {nestedProperty = Just "value3"})
        ,( "label4", {nestedProperty = Just "value4"})
        ,( "label5", {nestedProperty = Just "value5"})
        ]

dictWithRefJson =
    """
     {
       "label1": {},
       "label2": {},
       "label3": {},
       "label4": {},
       "label5": {}
     }

     """

expectedDictWithRef =
    Dict.fromList
        [( "label1", {})
        ,( "label2", {})
        ,( "label3", {})
        ,( "label4", {})
        ,( "label5", {})
        ]



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
        , test "should decode labels" <|
            always <|
                Expect.equal (Ok expectedLabels) (decodeString decodeLabels labelsJson)
        , test "should decode empty dict" <|
            always <|
                Expect.equal (Ok <| Dict.fromList []) (decodeString decodeLabels "{}")
        , test "should decode dict with nested object" <|
            always <|
                Expect.equal (Ok expectedDictWithObject) (decodeString decodeDictWithObject dictWithObjectJson)
        , test "should decode dict with nested ref" <|
            always <|
                Expect.equal (Ok expectedDictWithRef) (decodeString decodeDictWithRef dictWithRefJson)
        ]
