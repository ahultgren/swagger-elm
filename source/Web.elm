module Web exposing (..)

import Html exposing (Html, programWithFlags, text, div, textarea, h2, p, a)
import Html.Attributes exposing (class, style, href, target)
import Html.Events exposing (onInput)
import Generate exposing (generate)


type alias Flags =
    {}


type Model
    = NoOutput
    | Output (Result String String)


type Msg
    = Generate String


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( NoOutput, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate input ->
            ( Output <| generate input, Cmd.none )


boxStyles =
    style [ ( "width", "400px" ), ( "height", "400px" ), ( "margin-right", "20px" ), ( "font-family", "monospace" ) ]


view : Model -> Html Msg
view model =
    let
        output =
            case model of
                NoOutput ->
                    ""

                Output (Ok str) ->
                    str

                Output (Err err) ->
                    err
    in
        div [ style [ ( "margin", "10px" ) ] ]
            [ h2 [] [ text "Swagger elm demo" ]
            , p []
                [ text "Paste your json in the box to the left; get your Elm to the right. If you don't have your own json, try the "
                , a [ href "http://petstore.swagger.io/v2/swagger.json", target "_blank" ] [ text "petstore example" ]
                ]
            , textarea [ class "input", boxStyles, onInput Generate ] []
            , textarea [ class "output", boxStyles ] [ text output ]
            , p [] [ text "Learn more about ", a [ href "https://github.com/ahultgren/swagger-elm/" ] [ text "swagger elm" ] ]
            ]
