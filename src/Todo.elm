module Todo exposing (Todo)

import Dict exposing (Dict)
import TimeX exposing (Millis)


type Todo
    = Todo
        { id : String
        , title : String
        , body : String
        , done : Bool
        , createdAt : Millis
        , modifiedAt : Millis
        }


type Store
    = Store { lookup : Dict String Todo }
