port module Port exposing (cacheTodoStore, error, focusId, focusSelector)

import Json.Encode exposing (Value)


port cacheTodoStore : Value -> Cmd msg


port error : String -> Cmd msg


port focusId : String -> Cmd msg


port focusSelector : String -> Cmd msg
