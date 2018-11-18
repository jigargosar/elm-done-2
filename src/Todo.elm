module Todo exposing (Model, Msg(..), decoder, encoder, init, modify)

import BasicsX exposing (..)
import DecodeX exposing (Encoder)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E exposing (Value)
import TimeX exposing (Millis)


type alias Model =
    { id : String
    , title : String
    , body : String
    , done : Bool
    , createdAt : Millis
    , modifiedAt : Millis
    , contextId : String
    }


init model =
    model


encoder : Encoder Model
encoder model =
    E.object
        [ ( "id", E.string model.id )
        , ( "title", E.string model.title )
        , ( "body", E.string model.body )
        , ( "done", E.bool model.done )
        , ( "createdAt", E.int model.createdAt )
        , ( "modifiedAt", E.int model.modifiedAt )
        , ( "contextId", E.string model.contextId )
        ]


decoder : Decoder Model
decoder =
    DecodeX.start Model
        |> required "id" D.string
        |> required "title" D.string
        |> required "body" D.string
        |> required "done" D.bool
        |> required "createdAt" D.int
        |> required "modifiedAt" D.int
        |> required "contextId" D.string


type Msg
    = SetDone Bool


modify : Msg -> Model -> Model
modify message model =
    case message of
        SetDone done ->
            { model | done = done }
