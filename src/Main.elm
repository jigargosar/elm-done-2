module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import Browser
import BrowserX exposing (WindowSize)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json.Encode exposing (Value)
import TimeX exposing (Millis)



---- MODEL ----


type alias Flags =
    { now : Millis, windowSize : WindowSize, todos : Value, contexts : Value }


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
