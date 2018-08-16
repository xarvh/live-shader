module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onInput)
import ShaderParser
import Parser


type alias Model =
    String


init : Model
init =
    ""


type Msg
    = OnText String


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnText s ->
            s


view : Model -> Html Msg
view model =
    div
        []
        [ textarea
            [ onInput OnText
            , rows 10
            , cols 80
            ]
            []
        , div
            []
            [ model
                |> Parser.run ShaderParser.parse
                |> Debug.toString
                |> text
            ]
        ]
