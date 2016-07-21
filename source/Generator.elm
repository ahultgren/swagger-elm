module Generator exposing (..)

import String
import Dict
import Regex exposing (regex)
import Json.Decode exposing (decodeString)
import Decoder exposing (Swagger, decodeSwagger, Definition, Property)


type Type
    = String'
    | Int'
    | Float'
    | Bool'
    | Object'
    | Array' Definition
    | Ref' String
    | Unknown'


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map types


types : Swagger -> String
types { definitions } =
    String.concat <| List.map renderType <| Dict.toList definitions


renderType : ( String, Definition ) -> String
renderType ( name, definition ) =
    "type alias " ++ name ++ " = " ++ (renderFieldType True definition) ++ "\n"


renderProperties : Definition -> String
renderProperties { required, properties } =
    case properties of
        Just properties ->
            String.join ",\n" <| List.filterMap (renderProperty required) <| Dict.toList properties

        Nothing ->
            ""


renderProperty : Maybe (List String) -> ( String, Property ) -> Maybe String
renderProperty required ( name, Decoder.Property property ) =
    Just <| name ++ " : " ++ renderFieldType (isRequired required name) property


renderRefType : String -> String
renderRefType ref =
    let
        parsed =
            (List.head (Regex.find (Regex.AtMost 1) (regex "^#/definitions/(.+)$") ref))
                `Maybe.andThen` (List.head << .submatches)
    in
        case parsed of
            Just (Just ref') ->
                ref'

            _ ->
                Debug.crash "Unparseable reference " ++ ref


renderFieldType : Bool -> Definition -> String
renderFieldType isRequired' definition =
    let
        type' =
            case getType definition of
                String' ->
                    "String"

                Int' ->
                    "Int"

                Float' ->
                    "Float"

                Bool' ->
                    "Bool"

                Object' ->
                    "{\n" ++ renderProperties definition ++ "}\n"

                Array' definition' ->
                    -- TODO How to check if the array is required?
                    "List (" ++ (renderFieldType True definition') ++ ")"

                Ref' ref' ->
                    renderRefType ref'

                Unknown' ->
                    "TODO (Unknown)"
    in
        maybeWrap isRequired' type'


getType : Definition -> Type
getType { type', ref', items } =
    case ( type', ref', items ) of
        ( Just "array", _, Just (Decoder.Property items') ) ->
            Array' items'

        ( Just type', _, _ ) ->
            case type' of
                "string" ->
                    String'

                "integer" ->
                    Int'

                "number" ->
                    Float'

                "boolean" ->
                    Bool'

                "object" ->
                    Object'

                _ ->
                    Unknown'

        ( Nothing, Just ref', _ ) ->
            Ref' ref'

        ( Nothing, Nothing, Just (Decoder.Property items') ) ->
            Array' items'

        ( Nothing, Nothing, Nothing ) ->
            Object'


maybeWrap : Bool -> String -> String
maybeWrap isRequired type' =
    case isRequired of
        True ->
            type'

        False ->
            "Maybe (" ++ type' ++ ")"


isRequired : Maybe (List String) -> String -> Bool
isRequired required name =
    case required of
        Nothing ->
            False

        Just required ->
            let
                x =
                    Debug.log "name" ( name, required )
            in
                (not <| List.isEmpty <| List.filter ((==) name) required)
