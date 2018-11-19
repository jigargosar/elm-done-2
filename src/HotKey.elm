module HotKey exposing
    ( Event
    , SoftKey(..)
    , arrowDown
    , arrowUp
    , bindAll
    , bindEnter
    , bindEsc
    , decoder
    , enter
    , esc
    , initEvent
    , mapDecoder
    , onEnterDown
    , onEscDown
    , singletonBool
    , space
    )

import BasicsX exposing (..)
import EventX
import Html
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type SoftKey
    = Shift
    | Alt
    | Ctrl
    | Meta


type alias Event =
    ( List SoftKey, String )


type alias HotKey =
    ( List SoftKey, String )


singletonBool bool value =
    if bool then
        [ value ]

    else
        []


initEvent shift_ alt_ ctrl_ meta_ key =
    ( singletonBool shift_ Shift
        ++ singletonBool alt_ Alt
        ++ singletonBool ctrl_ Ctrl
        ++ singletonBool meta_ Meta
    , key
    )


decoder =
    D.map5 initEvent
        (D.field "shiftKey" D.bool)
        (D.field "altKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "metaKey" D.bool)
        (D.field "key" D.string)


mapDecoder : (Event -> msg) -> Decoder msg
mapDecoder tagger =
    D.map tagger decoder


bindAll : List ( HotKey, msg ) -> Decoder msg
bindAll mappings =
    decoder
        |> D.andThen
            (firstEq
                >> findIn mappings
                >> unwrapMaybe (D.fail "No Handler found") (Tuple.second >> D.succeed)
            )


enter : HotKey
enter =
    ( [], "Enter" )


esc : HotKey
esc =
    ( [], "Escape" )


arrowDown =
    ( [], "ArrowDown" )


arrowUp =
    ( [], "ArrowUp" )


space =
    ( [], " " )


bind : HotKey -> msg -> Decoder msg
bind hotkey tag =
    bindAll [ ( hotkey, tag ) ]


bindEnter : msg -> Decoder msg
bindEnter =
    bind enter


bindEsc : msg -> Decoder msg
bindEsc =
    bind esc


onEscDown : msg -> Html.Attribute msg
onEscDown =
    EventX.onKeyDown << bindEsc


onEnterDown : msg -> Html.Attribute msg
onEnterDown =
    EventX.onKeyDown << bindEnter
