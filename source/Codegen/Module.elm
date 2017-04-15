module Codegen.Module exposing (..)


header : String -> String -> String
header name exp =
    "module " ++ name ++ " exposing (" ++ exp ++ ")\n\n"
