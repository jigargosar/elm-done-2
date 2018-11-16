module HotKey exposing (Event, SoftKey(..), decoder, mapDecoder, onKeyDown)

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



--keyMap: List (Event, msg) ->
--keyMap mappings =


onKeyDown handler =
    Html.Events.on "keydown"
        (D.map
            handler
            decoder
        )
