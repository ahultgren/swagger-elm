module Generate.Swagger exposing (..)

import Codegen.Utils exposing (capitalize, sanitize)
import Generate.Type exposing (renderType)
import Generate.Decoder exposing (renderDecoder)
import Generate.Encoder exposing (renderEncoder)
import Swagger.Swagger exposing (Swagger)
import Swagger.Definition as Def exposing (Definition, getName)
import Generate.Headers exposing (renderHeaders)


render : Swagger -> String
render { definitions } =
    Def.map renderDefinition definitions
        |> String.concat
        |> (++) renderHeaders


renderDefinition : Definition -> String
renderDefinition definition =
    let
        name_ =
            moduleName <| getName definition
    in
        String.concat
            [ renderType definition
            , renderDecoder definition
            , renderEncoder definition
            , "\n\n"
            ]


moduleName : String -> String
moduleName =
    capitalize << sanitize
