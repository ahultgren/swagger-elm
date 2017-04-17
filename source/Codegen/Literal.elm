module Codegen.Literal exposing (..)


string : String -> String
string str =
    "\"" ++ str ++ "\""
