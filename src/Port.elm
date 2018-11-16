port module Port exposing (cacheTodoStore)

import Json.Encode exposing (Value)


port cacheTodoStore : Value -> Cmd msg
