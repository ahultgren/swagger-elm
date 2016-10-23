module Unit.Codegen.Utils exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, fail)
import Json.Decode exposing (decodeString)
import Codegen.Utils exposing (sanitize)


dirtyKeywords =
    [ "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "let"
    , "in"
    , "type"
    , "module"
    , "where"
    , "import"
    , "exposing"
    , "as"
    , "port"
    , "infix"
    , "infixl"
    , "infixr"
    ]


cleanKeywords =
    [ "if_"
    , "then_"
    , "else_"
    , "case_"
    , "of_"
    , "let_"
    , "in_"
    , "type_"
    , "module_"
    , "where_"
    , "import_"
    , "exposing_"
    , "as_"
    , "port_"
    , "infix_"
    , "infixl_"
    , "infixr_"
    ]


all : Test
all =
    describe "Decoder"
        [ describe "sanitize"
            [ test "removes symbols" <|
                always <|
                    Expect.equal "ref" <|
                        sanitize "$ref$!?+\"´`¨'*#%<>`/\\"
            , test "allows unicode letters" <|
                always <|
                    Expect.equal "örjan" <|
                        sanitize "örjan"
            , test "strips leading modifier or other letters" <|
                always <|
                    Expect.equal "öʰƻrjan" <|
                        sanitize "ʰƻöʰƻrjan"
            , test "allows underscore, but not leading" <|
                always <|
                    Expect.equal "under_score" <|
                        sanitize "_under_score"
            , test "allows numerals (and fancy numerals such as Ⅶ), but not leading" <|
                always <|
                    Expect.equal "KingGeorge0Ⅶ" <|
                        sanitize "Ⅶ0KingGeorge0Ⅶ"
            , test "strips multiple leading invalid characters" <|
                always <|
                    Expect.equal "aⅦ" <|
                        sanitize "_$ⅦaⅦ"
            , test "strips spaces" <|
                always <|
                    Expect.equal "MrBond" <|
                        sanitize " Mr. Bond "
            , test "allows ALLCAPS" <|
                always <|
                    Expect.equal "AAAARGGHHH" <|
                        sanitize "AAAARGGHHH!"
            , test "appends _ to keywords" <|
                always <|
                    Expect.equal cleanKeywords <|
                        List.map sanitize dirtyKeywords
            ]
        ]
