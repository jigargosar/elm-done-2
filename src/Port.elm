port module Port exposing (cacheTodoStore, error)

import Json.Encode exposing (Value)


port cacheTodoStore : Value -> Cmd msg


port error : String -> Cmd msg
