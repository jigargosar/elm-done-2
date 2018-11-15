module Todo exposing (Model)

import Dict exposing (Dict)
import Json.Decode as D
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
        ]



--type Store
--    = Store { lookup : Dict String Todo }
