module Todo exposing (Id, Model, Msg(..), decoder, done, encoder, idString, init, modify, setDone, title)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import TimeX exposing (Millis)


type alias Id =
    String


type alias ModelRecord =
    { id : String
    , title : String
    , body : String
    , done : Bool
    , createdAt : Millis
    , modifiedAt : Millis
    , contextId : String
    }


type Model
    = Model ModelRecord


init model =
    Model model


unWrap (Model model) =
    model


idString =
    unWrap >> .id


title =
    unWrap >> .title


done =
    unWrap >> .done


setDone val (Model model) =
    Model { model | done = val }


type alias Encoder =
    Model -> Value


encoder : Encoder
encoder (Model model) =
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
    D.map7 ModelRecord
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "body" D.string)
        (D.field "done" D.bool)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "contextId" D.string)
        |> D.map Model


type Msg
    = SetDone Bool


modify : Msg -> Model -> Model
modify message =
    case message of
        SetDone bool ->
            setDone bool
