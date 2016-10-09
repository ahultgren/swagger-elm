module Codegen.Utils exposing (capitalize, uncapitalize)

import String


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Just ( head, tail ) ->
            (String.toUpper <| String.fromChar head) ++ tail

        Nothing ->
            ""


uncapitalize : String -> String
uncapitalize str =
    case String.uncons str of
        Just ( head, tail ) ->
            (String.toLower <| String.fromChar head) ++ tail

        Nothing ->
            ""
