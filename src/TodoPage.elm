module TodoPage exposing
    ( Model
    , Msg(..)
    , empty
    , subscriptions
    , update
    , view
    )

import BasicsX exposing (..)
import Browser.Dom as Dom
import Browser.Events
import Cursor exposing (Cursor)
import El exposing (..)
import Element as E exposing (Element, clip, el, focused, fromRgb, fromRgb255, mouseOver, rgb, rgba, scrollbarY)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onFocus, onLoseFocus)
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import Fuzzy
import HotKey as HK
import Html.Attributes exposing (class, id, tabindex)
import Html.Events
import Icons
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import List as L
import List.Extra as L
import MaterialColorsUI exposing (..)
import Maybe as M
import Maybe.Extra as M
import Port
import Task
import Theme
import Todo exposing (Todo, TodoStore)
import TodoLI exposing (Item)
import Tuple exposing (mapFirst, second)
import UpdateX exposing (..)


type alias Model =
    { inputText : String
    , todoStore : TodoStore
    , inputHasFocus : Bool
    , cursor : Cursor
    , listHasFocus : Bool
    }


currentTodoList model =
    TodoLI.initList
        { query = model.inputText
        , todoStore = model.todoStore
        }


rollSelectionFocusBy offset model =
    let
        updatedModel =
            if model.listHasFocus || model.inputHasFocus then
                { model
                    | cursor = Cursor.rollBy offset (currentTodoList model) model.cursor
                }

            else
                model
    in
    pure updatedModel
        |> effect getScrollOrFocusSelectedCmd


debugNoOp x =
    let
        _ =
            Debug.log "debugNoOp" x
    in
    NoOp


getScrollOrFocusSelectedCmd model =
    if model.inputHasFocus then
        let
            scrollIntoViewCmd =
                getMaybeSelectedItem model
                    |> M.unwrap Cmd.none
                        (TodoLI.getSelectionIndicatorDomId
                            >> Dom.getElement
                            >> Task.attempt debugNoOp
                        )
        in
        scrollIntoViewCmd

    else
        Cmd.batch
            [ focusSelectedCmd model
            , todoListDomId
                |> Dom.getElement
                |> Task.attempt debugNoOp
            ]


getMaybeSelectedItem : Model -> Maybe Item
getMaybeSelectedItem model =
    Cursor.selected (currentTodoList model) model.cursor


focusSelectedCmd =
    getMaybeSelectedItem
        >> M.unwrap Cmd.none
            (TodoLI.getSelectionIndicatorDomId
                >> Port.focusId
            )


type FormMsg
    = InputChanged String
    | InputFocusChanged Bool
    | Submit
    | FormPD


type Msg
    = ---- INJECT MSG BELOW ----
      OnPrev
    | OnNext
    | TodoLIChanged Int TodoLI.Msg
    | FormChanged FormMsg
    | NewTodo Todo.TodoBuilder
    | LoadTS Value
    | NoOp


empty : Model
empty =
    { inputText = ""
    , todoStore = Todo.emptyStore
    , inputHasFocus = False
    , cursor = Cursor.empty
    , listHasFocus = False
    }


setInputText val model =
    { model | inputText = val }


resetInputText =
    setInputText ""


setCursorSelection : Cursor -> Model -> Model
setCursorSelection val model =
    { model | cursor = val }


setSelectionCursorAt =
    setCursorSelection << Cursor.initAt


resetSelectionCursor =
    setCursorSelection <| Cursor.empty


setInputHasFocus val model =
    { model | inputHasFocus = val }


setListHasFocus val model =
    { model | listHasFocus = val }


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <|
            HK.bindEachToMsg
                [ ( HK.arrowDown, OnNext )
                , ( HK.arrowUp, OnPrev )
                ]
        ]


defaultContextId =
    ""


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    (case message of
        ---- INJECT UPDATE CASE BELOW ----
        OnPrev ->
            rollSelectionFocusBy -1

        OnNext ->
            rollSelectionFocusBy 1

        TodoLIChanged idx msg ->
            case msg of
                TodoLI.Update todo modMsg ->
                    updateTS (Todo.update modMsg todo)
                        >> mapFirst (setSelectionCursorAt idx)

                TodoLI.RootClicked ->
                    setSelectionCursorAt idx
                        >> setListHasFocus True
                        >> pure

                TodoLI.RootFocusInChanged hasFocus ->
                    (if hasFocus then
                        setSelectionCursorAt idx

                     else
                        identity
                    )
                        >> setListHasFocus hasFocus
                        >> pure

                TodoLI.Create ->
                    onNewTodoMsg (Todo.initBuilder model.inputText defaultContextId)
                        >> mapModel resetInputText

                TodoLI.PD ->
                    pure

        FormChanged msg ->
            case msg of
                InputChanged value ->
                    setInputText value >> resetSelectionCursor >> pure

                InputFocusChanged hasFocus ->
                    pure << setInputHasFocus hasFocus

                Submit ->
                    performSelectedItemDefaultAction

                FormPD ->
                    pure

        NewTodo builder ->
            onNewTodoMsg builder

        LoadTS value ->
            updateTS (\_ -> Todo.loadStore value)

        NoOp ->
            pure
    )
    <|
        model


updateTS fn model =
    fn model.todoStore
        |> Tuple.mapFirst (\ts -> { model | todoStore = ts })


performSelectedItemDefaultAction : Model -> ( Model, Cmd Msg )
performSelectedItemDefaultAction model =
    let
        performItemAction item =
            (case item of
                TodoLI.FuzzyTodoLI todo ->
                    pure >> addCmd (focusSelectedCmd model)

                TodoLI.CreateTodoLI title ->
                    onNewTodoMsg (Todo.initBuilder model.inputText defaultContextId)
                        >> mapModel resetInputText
            )
            <|
                model
    in
    getMaybeSelectedItem model |> M.unwrap (pure model) performItemAction


onNewTodoMsg : Todo.TodoBuilder -> Model -> ( Model, Cmd Msg )
onNewTodoMsg =
    updateTS << Todo.new NewTodo


view : Model -> Element Msg
view model =
    c [ fw, clip, scrollbarY ]
        [ el [ p3, cx, fwx Theme.maxWidth ] <| viewInput model
        , el [ fHA <| id <| todoListScrollParentDomId, fw, clip, scrollbarY ] <|
            viewTodoList model
        ]


viewInput model =
    ip
        [ br2
        , p2
        , onLoseFocus <| InputFocusChanged False
        , onFocus <| InputFocusChanged True
        , onKeyDownPD <|
            HK.bindEachToMsg
                [ ( HK.arrowDown, ( FormPD, True ) )
                , ( HK.arrowUp, ( FormPD, True ) )
                , ( HK.enter, ( Submit, False ) )
                ]
        ]
        { onChange = InputChanged
        , text = model.inputText
        , placeholder = ipp [] (t "Add... / Search...")
        , label = lh "Task Title"
        }
        |> E.map FormChanged


viewTodoList : Model -> Element Msg
viewTodoList model =
    let
        viewItem idx selected item =
            TodoLI.view { hasFocus = model.listHasFocus, selected = selected, item = item }
                |> E.map (TodoLIChanged idx)

        viewItems =
            Cursor.selectionMap viewItem (currentTodoList model)
    in
    c [ fHA <| id <| todoListDomId, cx, fwx Theme.maxWidth ]
        (viewItems model.cursor)


todoListDomId =
    "todo-list-dom-id"


todoListScrollParentDomId =
    "todo-list-scroll-parent-dom-id"
