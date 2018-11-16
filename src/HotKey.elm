module HotKey exposing (Event, SoftKey(..), bindAll, bindEnter, bindEsc, decoder, enter, esc, initEvent, mapDecoder, onKeyDown, singletonBool)

import BasicsX exposing (..)
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


bindAll : List ( Event, msg ) -> Decoder msg
bindAll mappings =
    decoder
        |> D.andThen
            (firstEq
                >> findIn mappings
                >> unwrapMaybe (D.fail "No Handler found") (Tuple.second >> D.succeed)
            )


enter =
    ( [], "Enter" )


esc =
    ( [], "Escape" )


bind hotkey tag =
    bindAll [ ( hotkey, tag ) ]


bindEnter =
    bind enter


bindEsc =
    bind esc


onKeyDown handler =
    Html.Events.on "keydown"
        (D.map
            handler
            decoder
        )
