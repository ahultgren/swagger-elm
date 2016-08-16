module Swagger.Parse exposing (..)

import Dict
import String
import Swagger.Decode as Decode


parseDefinitions : Decode.Definitions -> Decode.Definitions
parseDefinitions definitions =
    extractNestedDefinitions definitions


extractNestedDefinitions : Decode.Definitions -> Decode.Definitions
extractNestedDefinitions definitions =
    Dict.toList definitions
        |> List.concatMap (extractNestedDefinition True)
        |> Dict.fromList


extractNestedDefinition : Bool -> ( String, Decode.Definition ) -> List ( String, Decode.Definition )
extractNestedDefinition isToplevel ( name, definition ) =
    case (definition.properties) of
        Nothing ->
            if isToplevel then
                [ ( name, definition ) ]
            else
                case definition.items of
                    Nothing ->
                        []

                    Just (Decode.Property items) ->
                        extractNestedDefinition False ( name, items )

        Just properties ->
            let
                children =
                    flattenProperties name properties
            in
                ( name, { definition | properties = children } )
                    :: (List.concatMap (extractNestedDefinition False << extractProperty name) <| Dict.toList properties)


flattenProperties : String -> Decode.Properties -> Maybe Decode.Properties
flattenProperties parentName properties =
    Just
        (Dict.toList properties
            |> List.map
                (\( name, Decode.Property definition ) ->
                    case definition.properties of
                        Nothing ->
                            ( name, Decode.Property definition )

                        Just props ->
                            ( name, Decode.Property (makeRef parentName name) )
                )
            |> Dict.fromList
        )


makeRef : String -> String -> Decode.Definition
makeRef parentName name =
    Decode.Definition Nothing [] Nothing Nothing <| Just <| "#/definitions/" ++ (nestedTypeName parentName name)


extractProperty : String -> ( String, Decode.Property ) -> ( String, Decode.Definition )
extractProperty parentName ( name, Decode.Property definition ) =
    ( nestedTypeName parentName name, definition )


nestedTypeName : String -> String -> String
nestedTypeName parentName name =
    parentName ++ "'" ++ (capitalize name)


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Just ( head, tail ) ->
            (String.toUpper <| String.fromChar head) ++ tail

        Nothing ->
            ""
