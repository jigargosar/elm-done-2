module TodoList exposing
    ( Model
    , Msg(..)
    , empty
    , update
    )

import BasicsX exposing (..)
import El exposing (..)
import Element exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import TodoStore
import UpdateX exposing (..)


type alias ModelRecord =
    { inputText : String
    , todoStore : TodoStore.Model
    }


type Model
    = Model ModelRecord


type Msg
    = InputChanged String
    | Submit
    | TodoStoreMsg TodoStore.Msg
    | LoadTodoStore Value


empty : Model
empty =
    Model
        { inputText = ""
        , todoStore = TodoStore.empty
        }



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
            mapModel (\(Model model) -> Model { model | inputText = value })

        Submit ->
            andThen (\(Model model) -> updateTodoStore (TodoStore.new model.inputText "") (Model model))

        TodoStoreMsg msg ->
            andThen <| updateTodoStore msg

        LoadTodoStore value ->
            andThen <| updateTodoStore (TodoStore.Load value)


updateTodoStore msg (Model model) =
    let
        ( todoStore, cmd ) =
            TodoStore.update msg model.todoStore
    in
    ( Model { model | todoStore = todoStore }, Cmd.map TodoStoreMsg cmd )



---- INJECT UPDATE CASE ABOVE ----


view : Model -> Element Msg
view (Model model) =
    el [] (t <| "Hello " ++ "TodoList")
