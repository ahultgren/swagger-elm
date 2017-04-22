module Generate exposing (main, generate)

import Html exposing (text)
import Html exposing (programWithFlags)
import Json.Decode as Decode exposing (decodeString)
import Swagger.Decode as Swagger exposing (decodeSwagger)
import Swagger.Flatten exposing (flatten)
import Generate.Swagger exposing (render)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


main : Program String String x
main =
    programWithFlags
        { init = init
        , view = text << toOutput << generate
        , update = always << always ( "", Cmd.none )
        , subscriptions = always Sub.none
        }


init : String -> ( String, Cmd x )
init json =
    ( json, Cmd.none )


toOutput : Result String String -> String
toOutput result =
    case result of
        Err err ->
            Debug.log "error" err

        Ok str ->
            str


generate : String -> Result String String
generate json =
    decodeString decodeSwagger json
        |> Result.map (flatten >> render)


type alias Comment =
    { responses : Responses
    }


type Responses
    = Responses (List Comment)


decodeComment : Decode.Decoder Comment
decodeComment =
    decode Comment
        |> required "responses" decodeResponses



-- SOLUTION


decodeResponses : Decode.Decoder Responses
decodeResponses =
    Decode.list (Decode.lazy (\_ -> decodeComment))
        |> Decode.map Responses


json : String
json =
    """
{
  "message": "asd",
  "upvotes": 0,
  "downvotes": 0,
  "responses": [
    {
      "message": "asd",
      "upvotes": 0,
      "downvotes": 0,
      "responses": [
        {
          "message": "asd",
          "upvotes": 0,
          "downvotes": 0,
          "responses": []
        }
      ]
    },
    {
      "message": "asd",
      "upvotes": 0,
      "downvotes": 0,
      "responses": []
    }
  ]
}
"""


test : Result String Comment
test =
    decodeString decodeComment json


maybe : String -> Decode.Decoder a -> Decode.Decoder (Maybe a -> b) -> Decode.Decoder b
maybe name decoder =
    optional name (Decode.map Just decoder) Nothing
