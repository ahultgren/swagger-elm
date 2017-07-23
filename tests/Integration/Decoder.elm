module Integration.Decoder exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Json.Decode exposing (decodeString)
import Dict
import Swagger
    exposing
        ( Article(Article)
        , ArticleDisplaySize(Large, Small)
        , ArticleNested(ArticleNested)
        , ArticleNestedGrandChildArray(ArticleNestedGrandChildArray)
        , ArticleNestedGrandChildArrayItem(ArticleNestedGrandChildArrayItem)
        , ArticleNestedGrandChildObject(ArticleNestedGrandChildObject)
        , ArticleUpperCasedField(ArticleUpperCasedField)
        , Comment(Comment)
        , DictWithObjectProperty(DictWithObjectProperty)
        , DictWithObject
        , DictWithRef
        , ErrorResponse(ErrorResponse)
        , Group(Group)
        , Labels
        , LowerCaseDefinitionObject(LowerCaseDefinitionObject)
        , Responses(Responses)
        , Rules(Rules)
        , decodeArticle
        , decodeErrorResponse
        , decodeComment
        , decodeGroup
        , decodeLabels
        , decodeDictWithObject
        , decodeDictWithRef
        , decodeRules
        )


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
  "rules": null
}
"""


expectedArticle : Article
expectedArticle =
    Article
        expectedArticleRecord


expectedArticleRecord =
    { id = "article1"
    , type_ = Just "article"
    , title = "Article one"
    , category_id = "blog"
    , displaySize = Large
    , nested =
        Just <|
            ArticleNested
                { one = Just "1"
                , two = Nothing
                , grandChildArray =
                    Just
                        (ArticleNestedGrandChildArray
                            [ ArticleNestedGrandChildArrayItem { grandAProp = Just "Array child one" }
                            , ArticleNestedGrandChildArrayItem { grandAProp = Just "Array child two" }
                            ]
                        )
                , grandChildObject = Just <| ArticleNestedGrandChildObject { grandOProp = Just 1.1 }
                , arrayOfStrings = Nothing
                }
    , rules = Just (Rules {})
    , sponsored = False
    , upperCasedField =
        ArticleUpperCasedField
            { upperCasedFieldSubfield = "test"
            }
    , lowerCaseDefinitionRef = Just True
    , lowerCaseDefinitionObjectRef = Just <| LowerCaseDefinitionObject {}
    , ref = Just "ref"
    }


errorResponseJson =
    """
{
  "message": "Everything went wrong",
  "level": 9000.1
}
"""


expectedErrorResponse : ErrorResponse
expectedErrorResponse =
    ErrorResponse
        { message = "Everything went wrong"
        , code = 0
        , level = 9000.1
        , readableCode = "fail"
        }


groupJson =
    "[" ++ articleJson ++ "]"


expectedGroup : Group
expectedGroup =
    Group
        [ expectedArticle ]


rulesJson =
    """
{
    "unknownField": "secret"
}
"""


expectedRules : Rules
expectedRules =
    Rules {}


labelsJson =
    """
 {
           "label1": "labelContent",
           "label2": "labelContent",
           "label3": "labelContent"
       }
     """


expectedLabels : Labels
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


expectedDictWithObject : DictWithObject
expectedDictWithObject =
    Dict.fromList
        [ ( "label1", DictWithObjectProperty { nestedProperty = Just "value1" } )
        , ( "label2", DictWithObjectProperty { nestedProperty = Just "value2" } )
        , ( "label3", DictWithObjectProperty { nestedProperty = Just "value3" } )
        , ( "label4", DictWithObjectProperty { nestedProperty = Just "value4" } )
        , ( "label5", DictWithObjectProperty { nestedProperty = Just "value5" } )
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


expectedDictWithRef : DictWithRef
expectedDictWithRef =
    Dict.fromList
        [ ( "label1", LowerCaseDefinitionObject {} )
        , ( "label2", LowerCaseDefinitionObject {} )
        , ( "label3", LowerCaseDefinitionObject {} )
        , ( "label4", LowerCaseDefinitionObject {} )
        , ( "label5", LowerCaseDefinitionObject {} )
        ]


commentJson =
    """
{
  "responses": [
    {
      "responses": [
        {
          "responses": []
        }
      ]
    },
    {
      "responses": []
    }
  ]
}
"""


expectedComment : Comment
expectedComment =
    Comment
        { responses =
            Responses
                [ Comment
                    { responses =
                        Responses
                            [ Comment
                                { responses = Responses []
                                }
                            ]
                    }
                , Comment
                    { responses = Responses []
                    }
                ]
        }


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
        , test "should decode comment (recursive)" <|
            always <|
                Expect.equal (Ok expectedComment) (decodeString decodeComment commentJson)
        ]
