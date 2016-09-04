module Codegen.Type exposing (..)

import String


type alias Name =
    String


type alias Body =
    String


type alias Type =
    String


type alias Properties =
    List Property


type alias Property =
    String


typeAlias : Name -> Body -> String
typeAlias name body =
    "type alias " ++ name ++ " = " ++ body ++ "\n"


unionType : Name -> Properties -> String
unionType name tags =
    "type "
        ++ name
        ++ "\n  = "
        ++ (String.join "\n  | " tags)
        ++ "\n"


record : Properties -> String
record properties =
    "{\n  " ++ (String.join "\n  ," properties) ++ "}"


recordField : Name -> Type -> String
recordField name type' =
    name ++ " : " ++ type'


list : Body -> String
list body =
    wrap "List" body


maybe : Body -> String
maybe body =
    wrap "Maybe" body


wrap : Name -> Body -> String
wrap name body =
    name ++ " (" ++ body ++ ")"
