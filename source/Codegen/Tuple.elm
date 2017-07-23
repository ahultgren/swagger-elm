module Codegen.Tuple exposing (..)


tuple : String -> String -> String
tuple fst snd =
    "( " ++ fst ++ ", " ++ snd ++ ")"
