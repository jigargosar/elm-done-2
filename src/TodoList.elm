module TodoList exposing
    ( Model
    , Msg(..)
    , empty
    , update
    , view
    )

import BasicsX exposing (..)
import El exposing (..)
import Element exposing (Element, el, fromRgb, fromRgb255, rgb)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import HotKey
import Icons
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MaterialColor exposing (blue50)
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
      OnDoneChanged Todo.Model Bool
    | InputChanged String
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
        OnDoneChanged todo bool ->
            andThen (updateTS <| TS.ModTodo (Todo.SetDone bool) todo)

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
    c [ fw ] (List.map viewTodo <| TS.list model.todoStore)


mcToEc { red, green, blue } =
    Element.rgb255 red green blue


viewTodo todo =
    r [ fw, bc <| mcToEc blue50 ]
        [ Input.checkbox [ pxy u3 u2, sw ]
            { label = lh "done"
            , icon =
                \checked ->
                    r [ fw, fh ]
                        [ ter checked Icons.checkCircleOutline Icons.circleOutline
                        ]
            , checked = Todo.done todo
            , onChange = OnDoneChanged todo
            }
        , el [ pxy u3 u2 ] (t <| Todo.title todo)
        ]
