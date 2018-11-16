---
inject: false
to: src/<%= module.replace(".","/") %>.elm
sh: elm-format --yes src/<%= module.replace(".","/") %>.elm
---
module <%= module %> exposing (Model, Msg(..), update, view)

import BasicsX exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import UpdateX exposing (..)
import El exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region

type alias ModelRecord =
    {
    }


type Model
    = Model ModelRecord


type Msg
    = NoOp

---- INJECT MSG ABOVE ----

update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    updateF message << pure


type alias ReturnF =
    ( Model, Cmd Msg ) -> ( Model, Cmd Msg )


updateF : Msg -> ReturnF
updateF message =
    case message of
        NoOp ->
            identity


---- INJECT UPDATE CASE ABOVE ----

view : Model -> Element Msg
view model =
  el [] (t <| "Hello " ++ "<%= module %>")
