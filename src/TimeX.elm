module TimeX exposing (Millis, now, nowMilli)

import Task exposing (Task)
import Time


type alias Millis =
    Int


nowMilli : Task x Millis
nowMilli =
    Task.map Time.posixToMillis Time.now


now : (Millis -> msg) -> Cmd msg
now toMsg =
    Task.perform toMsg nowMilli
