module HotKey exposing (Event, SoftKey(..), decoder, keyMap, mapDecoder, onKeyDown)

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


keyMap : List ( Event, msg ) -> Decoder msg
keyMap mappings =
    decoder
        |> D.andThen
            (firstEq
                >> findIn mappings
                >> unwrapMaybe (D.fail "No Handler found") (Tuple.second >> D.succeed)
            )


onKeyDown handler =
    Html.Events.on "keydown"
        (D.map
            handler
            decoder
        )
