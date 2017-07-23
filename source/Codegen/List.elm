module Codegen.List exposing (..)


list : List String -> String
list =
    String.join "\n  , "
        >> (++) "[ "
        >> flip (++) " ]"
