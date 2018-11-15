module Todo exposing (Model)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import TimeX exposing (Millis)


type Model
    = Model
        { id : String
        , title : String
        , body : String
        , done : Bool
        , createdAt : Millis
        , modifiedAt : Millis
        , contextId : String
        }


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
    D.map6 Model
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "body" D.string)
        (D.field "done" D.bool)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "contextId" D.string)
