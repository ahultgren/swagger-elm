module Tests exposing (..)

import Test exposing (..)
import Expect
import Codegen.Type exposing (record)


recordTest =
    record [ "prop1", "prop2" ]


recordFixture =
    """
  { prop1
  , prop2
  }
"""


all : Test
all =
    describe "Code.Type"
        [ describe ".record"
            [ test "should generate a record" <|
                \() ->
                    Expect.equal recordFixture recordTest
            ]
        ]
