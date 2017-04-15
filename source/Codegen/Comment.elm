module Codegen.Comment exposing (..)


lineComment : String -> String
lineComment comment =
    "--" ++ comment ++ "\n"
