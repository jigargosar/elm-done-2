module Todo exposing
    ( Msg(..)
    , Todo
    , TodoBuilder
    , TodoStore
    , all
    , emptyStore
    , initBuilder
    , loadStore
    , new
    , update
    )

import BasicsX exposing (unpackResult)
import DecodeX exposing (Encoder)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E exposing (Value)
import Port
import Random
import RandomId
import TimeX exposing (Millis)
import UpdateX exposing (..)


type alias Todo =
    { id : String
    , title : String
    , body : String
    , done : Bool
    , createdAt : Millis
    , modifiedAt : Millis
    , contextId : String
    }


type Msg
    = SetDone Bool


type alias TodoStore =
    { lookup : Dict String Todo
    }


emptyStore : TodoStore
emptyStore =
    { lookup = Dict.empty }


loadStore value =
    let
        todoDecoder : Decoder Todo
        todoDecoder =
            DecodeX.start Todo
                |> required "id" D.string
                |> required "title" D.string
                |> required "body" D.string
                |> required "done" D.bool
                |> required "createdAt" D.int
                |> required "modifiedAt" D.int
                |> required "contextId" D.string

        decoder : Decoder TodoStore
        decoder =
            D.map TodoStore
                (D.field "lookup" <| D.dict todoDecoder)
    in
    D.decodeValue decoder value
        |> unpackResult (\err -> ( emptyStore, Port.error <| "TodoStore: " ++ D.errorToString err )) pure


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
    case ( builder.id, builder.now ) of
        ( Nothing, _ ) ->
            ( model, RandomId.gen (msg << setJustId builder) )

        ( _, Nothing ) ->
            ( model, TimeX.now (msg << setJustNow builder) )

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
            upsertAndCache todo model


update : Msg -> Todo -> TodoStore -> ( TodoStore, Cmd msg )
update msg todo model =
    upsertAndCache (updateTodo msg (getOr todo model)) model


updateTodo : Msg -> Todo -> Todo
updateTodo message model =
    case message of
        SetDone done ->
            { model | done = done }


upsertAndCache todo model =
    pure { model | lookup = Dict.insert todo.id todo model.lookup }
        |> effect cache


cache =
    let
        todoEncoder : Encoder Todo
        todoEncoder model =
            E.object
                [ ( "id", E.string model.id )
                , ( "title", E.string model.title )
                , ( "body", E.string model.body )
                , ( "done", E.bool model.done )
                , ( "createdAt", E.int model.createdAt )
                , ( "modifiedAt", E.int model.modifiedAt )
                , ( "contextId", E.string model.contextId )
                ]

        encoder model =
            E.object
                [ ( "lookup", E.dict identity todoEncoder model.lookup )
                ]
    in
    Port.cacheTodoStore << encoder


all =
    .lookup >> Dict.values


getOr todo model =
    model.lookup |> Dict.get todo.id |> Maybe.withDefault todo
