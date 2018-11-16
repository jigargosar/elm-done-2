module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import Browser
import BrowserX exposing (WindowSize)
import El exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import HotKey
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Theme
import TimeX exposing (Millis)
import TodoStore
import UI



---- MODEL ----


type alias Flags =
    { now : Millis, windowSize : WindowSize, todos : Value, contexts : Value }


type alias Model =
    { inputText : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { inputText = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = InputChanged String
    | Submit



---- INJECT MSG ABOVE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        InputChanged value ->
            ( { model | inputText = value }, Cmd.none )

        Submit ->
            let
                _ =
                    Debug.log "model" model
            in
            ( model, Cmd.none )



---- INJECT UPDATE CASE ABOVE ----
---- VIEW ----


view : Model -> Html Msg
view model =
    UI.rootLayer
        [ inFront <|
            UI.layout
                { appBar = UI.appBar { title = UI.title2 "ELM" "DONE2" }
                , content = viewContent model
                }
        ]


viewContent model =
    c [ fw ]
        [ viewInput model
        , viewInput model
        , viewInput model
        ]


viewInput model =
    el [ p4 ]
        (Input.text
            [ Border.rounded u2, onKeyDown <| HotKey.keyMap [ HotKey.mapEnter Submit ] ]
            { onChange = InputChanged
            , text = model.inputText
            , placeholder = Just <| Input.placeholder [] (t "Title...")
            , label = Input.labelAbove [] (t "Task Title")
            }
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
