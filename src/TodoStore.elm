module TodoStore exposing
    ( Model
    , TodoBuilder
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
import Todo
import UpdateX exposing (..)


type alias Model =
    { lookup : Dict String Todo.Model
    }


empty : Model
empty =
    { lookup = Dict.empty }


type alias Encoder =
    Model -> Value


encoder : Encoder
encoder model =
    E.object
        [ ( "lookup", E.dict identity Todo.encoder model.lookup )
        ]


decoder : Decoder Model
decoder =
    D.map Model
        (D.field "lookup" <| D.dict Todo.decoder)


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


setJustId : HasMaybeIdNow x -> String -> HasMaybeIdNow x
setJustId model id =
    { model | id = Just id }


setJustNow : HasMaybeIdNow x -> Millis -> HasMaybeIdNow x
setJustNow model now =
    { model | now = Just now }


initBuilder title contextId =
    TodoBuilder Nothing Nothing title contextId



--modTodo : Todo.Msg -> Todo.Model -> ReturnF


modTodo : Todo.Msg -> Todo.Model -> Model -> ( Model, Cmd msg )
modTodo msg todo model =
    upsertAndCache (Todo.modify msg (getOr todo model)) <| pure model


restore value =
    D.decodeValue decoder value
        |> unpackResult (\err -> ( empty, Port.error <| "TodoStore: " ++ D.errorToString err )) pure


new : (TodoBuilder -> msg) -> TodoBuilder -> Model -> ( Model, Cmd msg )
new msg builder model =
    (case ( builder.id, builder.now ) of
        ( Nothing, _ ) ->
            addCmd <| RandomId.gen (msg << setJustId builder)

        ( _, Nothing ) ->
            addCmd <| TimeX.now (msg << setJustNow builder)

        ( Just id, Just now ) ->
            let
                todo : Todo.Model
                todo =
                    Todo.init
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



--upsertAndCache : Todo.Model -> ReturnF


upsertAndCache todo =
    mapModel (\model -> { model | lookup = Dict.insert (Todo.idString todo) todo model.lookup })
        >> effect (Port.cacheTodoStore << encoder)


all model =
    model.lookup |> Dict.values


getOr todo model =
    model.lookup |> Dict.get (Todo.idString todo) |> Maybe.withDefault todo
