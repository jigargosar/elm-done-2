module TodoStore exposing
    ( Model
    , Msg(..)
    , TodoBuilder
    , all
    , decodeOrEmpty
    , empty
    , initBuilder
    , new
    , onModTodoMsg
    , onNewMsg
    , onNewMsg2
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


type alias ModelRecord =
    { lookup : Dict String Todo.Model
    }


type Model
    = Model ModelRecord


empty : Model
empty =
    Model { lookup = Dict.empty }


decodeOrEmpty : Value -> ( Model, Cmd msg )
decodeOrEmpty =
    D.decodeValue decoder >> unpackResult (\err -> ( empty, Port.error "ERROR" )) pure


type alias Encoder =
    Model -> Value


encoder : Encoder
encoder (Model model) =
    E.object
        [ ( "lookup", E.dict identity Todo.encoder model.lookup )
        ]


decoder : Decoder Model
decoder =
    D.map ModelRecord
        (D.field "lookup" <| D.dict Todo.decoder)
        |> D.map Model


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


type Msg
    = New TodoBuilder
    | ModTodo Todo.Msg Todo.Model


new title contextId =
    New <| TodoBuilder Nothing Nothing title contextId


initBuilder title contextId =
    TodoBuilder Nothing Nothing title contextId


type alias ReturnF =
    ( Model, Cmd Msg ) -> ( Model, Cmd Msg )


onModTodoMsg msg todo =
    andThenF (\model -> upsertAndCache (Todo.modify msg (getOr todo model)))


restore value =
    D.decodeValue decoder value
        |> unpackResult (\err -> ( empty, Port.error <| "TodoStore: " ++ D.errorToString err )) pure


onNewMsg builder =
    case ( builder.id, builder.now ) of
        ( Nothing, _ ) ->
            addCmd <| RandomId.gen (New << setJustId builder)

        ( _, Nothing ) ->
            addCmd <| TimeX.now (New << setJustNow builder)

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


onNewMsg2 : (TodoBuilder -> msg) -> TodoBuilder -> Model -> ( Model, Cmd msg )
onNewMsg2 msg builder model =
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
    mapModel (\(Model model) -> Model { model | lookup = Dict.insert (Todo.idString todo) todo model.lookup })
        >> effect (Port.cacheTodoStore << encoder)


all (Model model) =
    model.lookup |> Dict.values


getOr todo (Model model) =
    model.lookup |> Dict.get (Todo.idString todo) |> Maybe.withDefault todo
