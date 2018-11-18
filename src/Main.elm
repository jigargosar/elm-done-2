module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import Browser
import BrowserX exposing (WindowSize)
import El exposing (..)
import Element exposing (Element, el, inFront)
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
import UI
import UpdateX exposing (..)



---- MODEL ----


type alias Flags =
    { now : Millis, windowSize : WindowSize, todos : Value, contexts : Value }


type alias Model =
    { todoList : TodoList.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    pure
        { todoList = TodoList.empty
        }
        |> andThen (updateTodoList <| TodoList.LoadTS flags.todos)



---- UPDATE ----


type Msg
    = ---- INJECT MSG BELOW ----
      TLMsg TodoList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    updateF message << pure


type alias ReturnF =
    ( Model, Cmd Msg ) -> ( Model, Cmd Msg )


updateF : Msg -> ReturnF
updateF message =
    case message of
        ---- INJECT UPDATE CASE BELOW ----
        TLMsg msg ->
            andThen (updateTodoList msg)


updateTodoList : TodoList.Msg -> Model -> ( Model, Cmd Msg )
updateTodoList msg model =
    let
        ( todoList, cmd ) =
            TodoList.update msg model.todoList
    in
    ( { model | todoList = todoList }, Cmd.map TLMsg cmd )


subscriptions model =
    Sub.batch [ TodoList.subscriptions model.todoList |> Sub.map TLMsg ]



---- VIEW ----


view : Model -> Html Msg
view model =
    UI.rootLayer
        [ inFront <|
            UI.layout
                { appBar = UI.appBar { title = UI.title2 "ELM" "DONE2" }
                , content = TodoList.view model.todoList |> eMap TLMsg
                }
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
