module Swagger.Flatten exposing (..)

import Dict
import String
import Swagger.Decode as Decode


flattenNestedDefinitions : Decode.Definitions -> Decode.Definitions
flattenNestedDefinitions definitions =
    Dict.toList definitions
        |> List.concatMap (flattenNestedDefinition True)
        |> Dict.fromList


flattenNestedDefinition : Bool -> ( String, Decode.Definition ) -> List ( String, Decode.Definition )
flattenNestedDefinition isToplevel ( name, definition ) =
    case (definition.properties) of
        Nothing ->
            if isToplevel then
                [ ( name, definition ) ]
            else
                case definition.items of
                    Nothing ->
                        []

                    Just (Decode.Property items) ->
                        flattenNestedDefinition False ( name, items )

        Just properties ->
            let
                children =
                    flattenProperties name properties
            in
                ( name, { definition | properties = children } )
                    :: (List.concatMap (flattenNestedDefinition False << fstNestedTypeName name << extractProperty) <| Dict.toList properties)


flattenProperties : String -> Decode.Properties -> Maybe Decode.Properties
flattenProperties parentName properties =
    Just
        (Dict.toList properties
            |> List.map
                (\( name, Decode.Property definition ) ->
                    case definition.properties of
                        Nothing ->
                            case definition.items of
                                Nothing ->
                                    ( name, Decode.Property definition )

                                Just items ->
                                    ( name, Decode.Property <| Decode.Definition (Just "array") [] Nothing (Just <| Decode.Property (makeRef parentName name)) Nothing Nothing )

                        Just props ->
                            ( name, Decode.Property (makeRef parentName name) )
                )
            |> Dict.fromList
        )


makeRef : String -> String -> Decode.Definition
makeRef parentName name =
    Decode.Definition Nothing [] Nothing Nothing (Just <| "#/definitions/" ++ (nestedTypeName parentName name)) Nothing


extractProperty : ( a, Decode.Property ) -> ( a, Decode.Definition )
extractProperty ( name, Decode.Property definition ) =
    ( name, definition )


fstNestedTypeName : String -> ( String, a ) -> ( String, a )
fstNestedTypeName parentName ( name, a ) =
    ( nestedTypeName parentName name, a )


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
