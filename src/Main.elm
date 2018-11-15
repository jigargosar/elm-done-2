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
import Theme
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
        [ Theme.baseFontFamily
        , Theme.baseFontSize
        , Element.inFront viewApp
        ]
        (Element.text "")


viewLogo =
    Element.image
        [ Element.width (Element.fill |> Element.maximum 24)
        ]
        { src = "/logo.svg", description = "Logo" }


viewAppBar =
    Element.el
        [ Element.width Element.fill
        , Background.color (Element.rgb 0 0 0)
        , Font.color (Element.rgb 1 1 1)
        ]
        (Element.row
            [ Element.width Element.fill
            , Element.paddingXY 8 8
            , Element.spacing 8
            ]
            [ viewLogo
            , Element.el
                []
                (Element.text "Logo")
            , Element.el
                [ Element.centerX ]
                (Element.text "ELM Done 2")
            ]
        )


viewApp =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewAppBar
        , viewLogo
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
