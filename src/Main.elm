module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import Browser
import BrowserX exposing (WindowSize)
import Element
import Html exposing (Html)
import Json.Encode exposing (Value)
import TimeX exposing (Millis)



---- MODEL ----


type alias Flags =
    { now : Millis, windowSize : WindowSize, todos : Value, contexts : Value }


type alias Model =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
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
    Element.layout []
        (Element.row []
            [ Element.image [] { src = "/logo.svg", description = "Logo" }
            , Element.row [] [ Element.text "Your Elm App is working!" ]
            ]
        )



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
