module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import Browser
import BrowserX exposing (WindowSize)
import El exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import HotKey
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Theme
import TimeX exposing (Millis)
import Todo
import TodoList
import TodoStore
import UI
import UpdateX exposing (..)



---- MODEL ----


type alias Flags =
    { now : Millis, windowSize : WindowSize, todos : Value, contexts : Value }


type alias Model =
    { inputText : String
    , todoStore : TodoStore.Model
    , todoList : TodoList.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    pure
        { inputText = ""
        , todoStore = TodoStore.empty
        , todoList = TodoList.empty
        }
        |> andThen (updateTodoStore <| TodoStore.Load flags.todos)
        |> andThen (updateTodoList <| TodoList.LoadTodoStore flags.todos)



---- UPDATE ----


type Msg
    = InputChanged String
    | Submit
    | TodoStoreMsg TodoStore.Msg
    | TodoListMsg TodoList.Msg



---- INJECT MSG ABOVE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    updateF message << pure


type alias ReturnF =
    ( Model, Cmd Msg ) -> ( Model, Cmd Msg )


updateF : Msg -> ReturnF
updateF message =
    case message of
        InputChanged value ->
            mapModel (\model -> { model | inputText = value })

        Submit ->
            andThen (\model -> updateTodoStore (TodoStore.new model.inputText "") model)

        TodoStoreMsg msg ->
            andThen (updateTodoStore msg)

        TodoListMsg msg ->
            andThen (updateTodoList msg)


updateTodoStore msg model =
    let
        ( todoStore, cmd ) =
            TodoStore.update msg model.todoStore
    in
    ( { model | todoStore = todoStore }, Cmd.map TodoStoreMsg cmd )


updateTodoList msg model =
    let
        ( todoList, cmd ) =
            TodoList.update msg model.todoList
    in
    ( { model | todoList = todoList }, Cmd.map TodoListMsg cmd )



---- INJECT UPDATE CASE ABOVE ----
---- VIEW ----


view : Model -> Html Msg
view model =
    UI.rootLayer
        [ inFront <|
            UI.layout
                { appBar = UI.appBar { title = UI.title2 "ELM" "DONE2" }
                , content = TodoList.view model.todoList |> Element.map TodoListMsg
                }
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
