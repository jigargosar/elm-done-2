port module Port exposing (cacheTodoStore, error, focusId)

import Json.Encode exposing (Value)


port cacheTodoStore : Value -> Cmd msg


port error : String -> Cmd msg


port focusId : String -> Cmd msg
