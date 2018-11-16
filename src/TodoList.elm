module TodoList exposing
    ( Model
    , Msg(..)
    , empty
    , update
    , view
    )

import BasicsX exposing (..)
import El exposing (..)
import Element exposing (Element, el)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import HotKey
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Todo
import TodoStore as TS
import UpdateX exposing (..)


type alias ModelRecord =
    { inputText : String
    , todoStore : TS.Model
    }


type Model
    = Model ModelRecord


unWrap (Model model) =
    model


type Msg
    = ---- INJECT MSG BELOW ----
      InputChanged String
    | Submit
    | TSMsg TS.Msg
    | LoadTS Value


empty : Model
empty =
    Model
        { inputText = ""
        , todoStore = TS.empty
        }


setInputText val (Model model) =
    Model { model | inputText = val }


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    updateF message << pure


type alias ReturnF =
    ( Model, Cmd Msg ) -> ( Model, Cmd Msg )


updateF : Msg -> ReturnF
updateF message =
    case message of
        ---- INJECT UPDATE CASE BELOW ----
        InputChanged value ->
            mapModel (setInputText value)

        Submit ->
            andThenF (unWrap >> (\model -> updateTS (TS.new model.inputText "")))
                >> mapModel (setInputText "")

        TSMsg msg ->
            andThen <| updateTS msg

        LoadTS value ->
            andThen <| updateTS (TS.Load value)


updateTS msg (Model model) =
    let
        ( todoStore, cmd ) =
            TS.update msg model.todoStore
    in
    ( Model { model | todoStore = todoStore }, Cmd.map TSMsg cmd )


view : Model -> Element Msg
view (Model model) =
    c [ fw ]
        [ viewInput model
        , viewTodoList model
        ]


viewInput model =
    el [ p3 ]
        (ip
            [ br2, onEnterDown Submit, p2 ]
            { onChange = InputChanged
            , text = model.inputText
            , placeholder = ipp [] (t "Title...")
            , label = lh "Task Title"
            }
        )


viewTodoList model =
    c [] (List.map viewTodo <| TS.list model.todoStore)


viewTodo todo =
    el [] (t <| Todo.title todo)
