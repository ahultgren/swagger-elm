module Codegen.Function exposing (function, arg, pipeline, letin, caseof, lazy)

import String


type alias Args =
    List Arg


type Arg
    = Arg Type Name


type alias Type =
    String


type alias Name =
    String


type alias Body =
    String


function : Name -> Args -> Type -> Body -> String
function name args type_ body =
    (functionType name args type_ body)
        ++ "\n"
        ++ (functionDeclaration name args type_ body)
        ++ "\n"


functionType : Name -> Args -> Type -> Body -> String
functionType name args type_ body =
    name
        ++ " : "
        ++ (String.concat <| List.map ((flip (++) " -> ") << argType) args)
        ++ type_


functionDeclaration : Name -> Args -> Type -> Body -> String
functionDeclaration name args type_ body =
    name ++ " " ++ (String.join " " <| List.map argName args) ++ " = \n" ++ body


arg : Type -> Name -> Arg
arg type_ name =
    Arg type_ name


argType : Arg -> Type
argType (Arg type_ name) =
    type_


argName : Arg -> Name
argName (Arg type_ name) =
    name


type alias Initial =
    String


type alias Piped =
    String


pipeline : Initial -> List Piped -> String
pipeline init items =
    items
        |> List.map ((++) "\n  |> ")
        |> String.concat
        |> (++) init


letin : List ( Name, Body ) -> Body -> String
letin declarations body =
    let
        lets =
            declarations
                |> List.map (\( name, body ) -> "      " ++ name ++ " = " ++ body)
                |> String.join "\n"
    in
        "  let\n" ++ lets ++ "\n    in\n      " ++ body


caseof : String -> List ( Name, Body ) -> String
caseof case_ conditions =
    let
        conds =
            conditions
                |> List.map (\( name, body ) -> "      " ++ name ++ " -> " ++ body)
                |> String.join "\n"
    in
        "  case " ++ case_ ++ " of\n" ++ conds


lazy : Body -> String
lazy body =
    "(lazy (\\_ -> " ++ body ++ "))"
