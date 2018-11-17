module TodoList exposing
    ( Model
    , Msg(..)
    , empty
    , update
    , view
    )

import BasicsX exposing (..)
import El exposing (..)
import Element exposing (Element, el, fromRgb, fromRgb255, rgb, rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import HotKey
import Icons
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MaterialColorsElmUI exposing (..)
import Todo
import TodoStore as TS
import UpdateX exposing (..)


type alias ModelRecord =
    { inputText : String
    , todoStore : TS.Model
    , selectedIdx : Maybe Int
    }


type Model
    = Model ModelRecord


unwrap (Model model) =
    model


inputText =
    unwrap >> .inputText


todoStore =
    unwrap >> .todoStore


currentList (Model model) =
    let
        filteredList =
            TS.all model.todoStore
    in
    if List.isEmpty filteredList then
        Nothing

    else
        let
            idxMax =
                List.length filteredList - 1
        in
        model.selectedIdx |> unwrapMaybe 0 (min idxMax) |> (\idx -> Just ( idx, filteredList ))


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
        , selectedIdx = Nothing
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
            updateTS <| TS.ModTodo (Todo.SetDone bool) todo

        InputChanged value ->
            mapModel (setInputText value)

        Submit ->
            andThenF (\model -> updateTS (TS.new (inputText model) ""))
                >> mapModel (setInputText "")

        TSMsg msg ->
            updateTS msg

        LoadTS value ->
            updateTS (TS.Load value)


updateTS msg =
    andThen
        (\(Model model) ->
            let
                ( todoStore_, cmd ) =
                    TS.update msg model.todoStore
            in
            ( Model { model | todoStore = todoStore_ }, Cmd.map TSMsg cmd )
        )


view : Model -> Element Msg
view model =
    c [ fw ]
        [ viewInput model
        , viewTodoList model
        ]


viewInput model =
    el [ p3 ]
        (ip
            [ br2, onEnterDown Submit, p2 ]
            { onChange = InputChanged
            , text = inputText model
            , placeholder = ipp [] (t "Title...")
            , label = lh "Task Title"
            }
        )


viewTodoList model =
    case currentList model of
        Nothing ->
            c [ fw ] [ t "No Tasks Found" ]

        Just ( si, todos ) ->
            c [ fw ] (List.indexedMap (viewTodo si) todos)


viewTodo si idx todo =
    let
        isSelected =
            idx == si

        selectionIndicator =
            el
                [ bw 2
                , fh
                , bcIf isSelected blue300
                ]
                (t "")
    in
    r
        [ fw
        , bgc blue50
        ]
        [ selectionIndicator
        , Input.checkbox [ pxy u3 u2, sw ]
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
