module TodoList exposing
    ( Model
    , Msg(..)
    , empty
    , subscriptions
    , update
    , view
    )

import BasicsX exposing (..)
import Browser.Events
import El exposing (..)
import Element as E exposing (Element, clip, el, focused, fromRgb, fromRgb255, mouseOver, rgb, rgba, scrollbarY)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onFocus, onLoseFocus)
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import Fuzzy
import HotKey
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
import SelectionList exposing (Selection, SelectionList)
import Theme
import Todo exposing (Todo, TodoStore)
import TodoLI
import Tuple exposing (second)
import UpdateX exposing (..)


type alias Model =
    { inputText : String
    , todoStore : TodoStore
    , inputHasFocus : Bool
    , selection : Selection
    }


filterWithFuzzyResult : String -> List Todo -> List ( Fuzzy.Result, Todo )
filterWithFuzzyResult query =
    let
        boil =
            String.toLower

        fuzzMatcher todo =
            Fuzzy.match [] [] (boil query) (boil <| todo.title)
                |> justWhen (.score >> flip (<) 1000)

        filterMapFn todo =
            fuzzMatcher todo
                |> Maybe.map (\res -> ( res, todo ))

        sort =
            L.sortBy (Tuple.first >> .score)
    in
    L.filterMap filterMapFn
        >> unlessBool (isBlank query) sort


todoSelectionList : Model -> SelectionList ( Fuzzy.Result, Todo )
todoSelectionList model =
    let
        filteredList =
            Todo.all model.todoStore
                |> filterWithFuzzyResult model.inputText

        selectionList : SelectionList ( Fuzzy.Result, Todo )
        selectionList =
            SelectionList.withList filteredList model.selection
    in
    selectionList


rollSelectionBy offset model =
    let
        selectionList =
            todoSelectionList model
                |> SelectionList.rollBy offset

        selection =
            selectionList |> SelectionList.toSelection

        focusSelectedCmd : Cmd Msg
        focusSelectedCmd =
            SelectionList.getSelectedItem selectionList
                |> M.unwrap Cmd.none
                    (second
                        >> .id
                        >> todoItemDomId
                        >> (\domId -> Port.focusSelector ("#" ++ domId ++ " ." ++ TodoLI.xSelectionIndicator))
                    )
    in
    --    ( { model | selection = selection }, focusSelectedCmd )
    pure { model | selection = selection }


type FormMsg
    = InputChanged String
    | InputFocusChanged Bool
    | Submit
    | FormPD


type Msg
    = ---- INJECT MSG BELOW ----
      OnPrev
    | OnNext
    | TodoLIChange Int Todo TodoLI.Msg
    | FormChange FormMsg
    | NewTodo Todo.TodoBuilder
    | LoadTS Value


empty : Model
empty =
    { inputText = ""
    , todoStore = Todo.emptyStore
    , inputHasFocus = False
    , selection = SelectionList.emptySelection
    }


setInputText val model =
    { model | inputText = val }


setFixedSelection val model =
    { model | selection = SelectionList.fixedSelection val }


resetSelection model =
    { model | selection = SelectionList.emptySelection }


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <|
            HotKey.bindAll
                [ ( HotKey.arrowDown, OnNext )
                , ( HotKey.arrowUp, OnPrev )
                ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ---- INJECT UPDATE CASE BELOW ----
        OnPrev ->
            --            updateSelectedIdxBy ((+) -1) model
            rollSelectionBy -1 model

        OnNext ->
            --            updateSelectedIdxBy ((+) 1) model
            rollSelectionBy 1 model

        TodoLIChange idx todo msg ->
            case msg of
                TodoLI.Update modMsg ->
                    updateTS (Todo.update modMsg todo) model

                TodoLI.RootClicked ->
                    pure <| setFixedSelection idx model

                TodoLI.PD ->
                    ( model, Cmd.none )

        FormChange msg ->
            case msg of
                InputChanged value ->
                    pure <| (setInputText value model |> resetSelection)

                InputFocusChanged hasFocus ->
                    pure <| { model | inputHasFocus = hasFocus }

                Submit ->
                    onNewTodoMsg (Todo.initBuilder model.inputText "") model
                        |> mapModel (setInputText "")

                FormPD ->
                    ( model, Cmd.none )

        NewTodo builder ->
            onNewTodoMsg builder model

        LoadTS value ->
            updateTS (\_ -> Todo.loadStore value) model


updateTS fn model =
    fn model.todoStore
        |> Tuple.mapFirst (\ts -> { model | todoStore = ts })


onNewTodoMsg : Todo.TodoBuilder -> Model -> ( Model, Cmd Msg )
onNewTodoMsg =
    updateTS << Todo.new NewTodo


view : Model -> Element Msg
view model =
    c [ fw, clip, scrollbarY ]
        [ el [ p3, cx, fwx Theme.maxWidth ] <| viewInput model
        , el [ fw, clip, scrollbarY ] <|
            c [ cx, fwx Theme.maxWidth ] <|
                --                case maybeTodoListViewModel model of
                --                    Nothing ->
                --                        [ t "No Tasks Found" ]
                --
                --                    Just viewModel ->
                --                        viewTodoList viewModel
                viewTodoListChildren
                <|
                    todoSelectionList model
        ]


viewInput model =
    ip
        [ br2
        , p2
        , onLoseFocus <| InputFocusChanged False
        , onFocus <| InputFocusChanged True
        , onKeyDownPDBindAll
            [ ( HotKey.arrowDown, ( FormPD, True ) )
            , ( HotKey.arrowUp, ( FormPD, True ) )
            , ( HotKey.enter, ( Submit, False ) )
            ]
        ]
        { onChange = InputChanged
        , text = model.inputText
        , placeholder = ipp [] (t "Add... / Search...")
        , label = lh "Task Title"
        }
        |> E.map FormChange


todoItemDomId id =
    "todo-li--" ++ id


viewTodoListChildren selectionList =
    let
        viewChild idx isSelected ( matchResult, todo ) =
            TodoLI.view
                { selected = isSelected
                , todoId = todo.id
                , done = todo.done
                , title = todo.title
                }
                |> E.map (TodoLIChange idx todo)
    in
    SelectionList.selectionMap viewChild selectionList
