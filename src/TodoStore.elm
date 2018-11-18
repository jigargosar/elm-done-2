module TodoStore exposing
    ( TodoBuilder
    , TodoStore
    , all
    , empty
    , initBuilder
    , modTodo
    , new
    , restore
    )

import BasicsX exposing (unpackResult)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Port
import Random
import RandomId
import TimeX exposing (Millis)
import Todo exposing (Todo)
import UpdateX exposing (..)


type alias TodoStore =
    { lookup : Dict String Todo
    }


empty : TodoStore
empty =
    { lookup = Dict.empty }


type alias TodoBuilder =
    { id : Maybe String
    , now : Maybe Millis
    , title : String
    , contextId : String
    }


type alias HasMaybeIdNow x =
    { x
        | id : Maybe String
        , now : Maybe Millis
    }


initBuilder title contextId =
    TodoBuilder Nothing Nothing title contextId


modTodo : Todo.Msg -> Todo -> TodoStore -> ( TodoStore, Cmd msg )
modTodo msg todo model =
    upsertAndCache (Todo.modify msg (getOr todo model)) <| pure model


restore value =
    let
        decoder : Decoder TodoStore
        decoder =
            D.map TodoStore
                (D.field "lookup" <| D.dict Todo.decoder)
    in
    D.decodeValue decoder value
        |> unpackResult (\err -> ( empty, Port.error <| "TodoStore: " ++ D.errorToString err )) pure


new : (TodoBuilder -> msg) -> TodoBuilder -> TodoStore -> ( TodoStore, Cmd msg )
new msg builder model =
    let
        setJustId : HasMaybeIdNow x -> String -> HasMaybeIdNow x
        setJustId hasIdNow id =
            { hasIdNow | id = Just id }

        setJustNow : HasMaybeIdNow x -> Millis -> HasMaybeIdNow x
        setJustNow hasIdNow now =
            { hasIdNow | now = Just now }
    in
    (case ( builder.id, builder.now ) of
        ( Nothing, _ ) ->
            addCmd <| RandomId.gen (msg << setJustId builder)

        ( _, Nothing ) ->
            addCmd <| TimeX.now (msg << setJustNow builder)

        ( Just id, Just now ) ->
            let
                todo : Todo
                todo =
                    { id = id
                    , createdAt = now
                    , modifiedAt = now
                    , title = builder.title
                    , body = ""
                    , done = False
                    , contextId = builder.contextId
                    }
            in
            upsertAndCache todo
    )
    <|
        pure model


upsertAndCache todo =
    mapModel (\model -> { model | lookup = Dict.insert todo.id todo model.lookup })
        >> effect cache


cache =
    let
        encoder model =
            E.object
                [ ( "lookup", E.dict identity Todo.encoder model.lookup )
                ]
    in
    Port.cacheTodoStore << encoder


all =
    .lookup >> Dict.values


getOr todo model =
    model.lookup |> Dict.get todo.id |> Maybe.withDefault todo
