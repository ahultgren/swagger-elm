module Codegen.Type exposing (..)

import String
import Codegen.Utils exposing (capitalize, uncapitalize)


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
    "type alias " ++ (capitalize name) ++ " = " ++ body ++ "\n"


unionType : Name -> Properties -> String
unionType name tags =
    "type "
        ++ (capitalize name)
        ++ "\n  = "
        ++ (String.join "\n  | " tags)
        ++ "\n"


record : Properties -> String
record properties =
    "\n  { " ++ (String.join "\n  , " properties) ++ "\n  }\n"


recordField : Name -> Type -> String
recordField name type_ =
    (uncapitalize name) ++ " : " ++ type_


list : Body -> String
list body =
    wrap "List" body


maybe : Body -> String
maybe body =
    wrap "Maybe" body


wrap : Name -> Body -> String
wrap name body =
    name ++ " (" ++ body ++ ")"
