module HotKey exposing
    ( Event
    , SoftKey(..)
    , arrowDown
    , arrowUp
    , bindAll
    , decoder
    , enter
    , esc
    , initHotKey
    , mapDecoder
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


initHotKey shift_ alt_ ctrl_ meta_ key =
    ( singletonBool shift_ Shift
        ++ singletonBool alt_ Alt
        ++ singletonBool ctrl_ Ctrl
        ++ singletonBool meta_ Meta
    , key
    )


fromKeyEvent { shiftKey, altKey, ctrlKey, metaKey, key } =
    ( singletonBool shiftKey Shift
        ++ singletonBool altKey Alt
        ++ singletonBool ctrlKey Ctrl
        ++ singletonBool metaKey Meta
    , key
    )


decoder : Decoder HotKey
decoder =
    D.map fromKeyEvent EventX.keyEventDecoder



--    D.map5 initHotKey
--        (D.field "shiftKey" D.bool)
--        (D.field "altKey" D.bool)
--        (D.field "ctrlKey" D.bool)
--        (D.field "metaKey" D.bool)
--        (D.field "key" D.string)


mapDecoder : (HotKey -> msg) -> Decoder msg
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
