module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import Browser
import BrowserX exposing (WindowSize)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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
    Element.layout
        [ Element.inFront viewApp
        ]
        (Element.text "")


viewAppBar =
    Element.el
        [ Element.width Element.fill
        , Background.color (Element.rgb 0 0 0)
        , Font.color (Element.rgb 1 1 1)
        ]
        (Element.row
            [ Element.width Element.fill, Element.paddingXY 8 8 ]
            [ Element.el
                []
                (Element.text "Logo")
            , Element.el
                [ Element.centerX, Element.centerY, Element.height Element.fill ]
                (Element.text "ELM Done 2")
            ]
        )


viewApp =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewAppBar
        , Element.image
            [ Element.width (Element.fill |> Element.maximum 48)
            ]
            { src = "/logo.svg", description = "Logo" }
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
