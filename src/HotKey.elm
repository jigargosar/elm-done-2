module HotKey exposing
    ( Event
    , SoftKey(..)
    , arrowDown
    , arrowUp
    , bindEachToMsg
    , decoder
    , enter
    , esc
    , space
    )

import BasicsX exposing (..)
import EventX exposing (KeyEvent)
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


appendWhen bool value list =
    if bool then
        list ++ [ value ]

    else
        list


fromKeyEvent : KeyEvent -> HotKey
fromKeyEvent { shiftKey, altKey, ctrlKey, metaKey, key } =
    ( []
        |> appendWhen shiftKey Shift
        |> appendWhen altKey Alt
        |> appendWhen ctrlKey Ctrl
        |> appendWhen metaKey Meta
    , key
    )


matchesKeyEvent : KeyEvent -> HotKey -> Bool
matchesKeyEvent ke =
    eq (fromKeyEvent ke)


decoder : Decoder HotKey
decoder =
    D.map fromKeyEvent EventX.keyEventDecoder


bindEachToMsg : List ( HotKey, msg ) -> Decoder msg
bindEachToMsg mappings =
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


arrowDown : HotKey
arrowDown =
    ( [], "ArrowDown" )


arrowUp : HotKey
arrowUp =
    ( [], "ArrowUp" )


space : HotKey
space =
    ( [], " " )
