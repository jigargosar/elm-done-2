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
import Html.Attributes exposing (id, tabindex)
import Html.Events
import Icons
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import List as L
import List.Extra as L
import List.Selection as Selection exposing (Selection)
import MaterialColorsUI exposing (..)
import Maybe as M
import Maybe.Extra as M
import Port
import Theme
import Todo exposing (Todo, TodoStore)
import UpdateX exposing (..)


type alias Model =
    { inputText : String
    , todoStore : TodoStore
    , maybeIdx : Maybe Int
    , inputHasFocus : Bool
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


todoSelectionList : Model -> Maybe (Selection ( Fuzzy.Result, Todo ))
todoSelectionList model =
    let
        filteredList =
            Todo.all model.todoStore
                |> filterWithFuzzyResult model.inputText

        maybeSelected =
            model.maybeIdx |> M.withDefault 0 |> atClampedIdx filteredList

        noSelectionList =
            Selection.fromList filteredList
    in
    maybeSelected
        |> M.map (\item -> Selection.select item noSelectionList)


maybeTodoListViewModel : Model -> Maybe ( Int, List ( Fuzzy.Result, Todo ) )
maybeTodoListViewModel model =
    let
        filteredList =
            Todo.all model.todoStore
                |> filterWithFuzzyResult model.inputText
    in
    if L.isEmpty filteredList then
        Nothing

    else
        let
            idxMax =
                L.length filteredList - 1
        in
        model.maybeIdx |> unwrapMaybe 0 (min idxMax) |> (\idx -> Just ( idx, filteredList ))


updateSelectedIdxBy numFn model =
    maybeTodoListViewModel model
        |> unwrapMaybe (pure model)
            (\( si, tl ) ->
                let
                    newIdx =
                        safeModBy (L.length tl) (numFn si)

                    --                  _  =
                    --                    Array.fromList
                    --                      |> L.head
                    --                      |> Maybe.map (Tuple.second >> .id)
                    --
                in
                ( numFn si, L.length tl )
                    |> (\( idx, length ) -> safeModBy length idx)
                    >> (\idx -> ( { model | maybeIdx = Just idx }, Cmd.none ))
            )


type FormMsg
    = InputChanged String
    | InputFocusChanged Bool


type Msg
    = ---- INJECT MSG BELOW ----
      OnPrev
    | OnNext
    | UpdateTodo Todo Todo.Msg
    | FormChange FormMsg
    | SelectTodo Todo
    | SetSelectionIdx Int
    | TodoRootClicked Todo
    | FocusId String
    | Submit
    | NewTodo Todo.TodoBuilder
    | LoadTS Value
    | NoOp


empty : Model
empty =
    { inputText = ""
    , todoStore = Todo.emptyStore
    , maybeIdx = Nothing
    , inputHasFocus = False
    }


setInputText val model =
    { model | inputText = val }


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
            updateSelectedIdxBy ((+) -1) model

        OnNext ->
            updateSelectedIdxBy ((+) 1) model

        UpdateTodo todo msg ->
            updateTS (Todo.update msg todo) model

        SetSelectionIdx idx ->
            ( { model | maybeIdx = Just idx }, Cmd.none )

        SelectTodo todo ->
            ( model, Cmd.none )

        TodoRootClicked todo ->
            ( model, Cmd.none )

        FocusId domId ->
            ( model, Port.focusId domId )

        FormChange msg ->
            case msg of
                InputChanged value ->
                    pure <| setInputText value model

                InputFocusChanged hasFocus ->
                    pure <| { model | inputHasFocus = hasFocus }

        Submit ->
            onNewTodoMsg (Todo.initBuilder model.inputText "") model
                |> mapModel (setInputText "")

        NewTodo builder ->
            onNewTodoMsg builder model

        LoadTS value ->
            updateTS (\_ -> Todo.loadStore value) model

        NoOp ->
            pure model


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
                case maybeTodoListViewModel model of
                    Nothing ->
                        [ t "No Tasks Found" ]

                    Just viewModel ->
                        viewTodoList viewModel
        ]


viewInput model =
    ip
        [ br2
        , p2
        , onLoseFocus <| FormChange <| InputFocusChanged False
        , onFocus <| FormChange <| InputFocusChanged True
        , onKeyDownPDBindAll
            [ ( HotKey.arrowDown, ( NoOp, True ) )
            , ( HotKey.arrowUp, ( NoOp, True ) )
            , ( HotKey.enter, ( Submit, False ) )
            ]
        ]
        { onChange = FormChange << InputChanged
        , text = model.inputText
        , placeholder = ipp [] (t "Add... / Search...")
        , label = lh "Task Title"
        }


todoItemDomId id =
    "todo--" ++ id


viewTodoList ( maybeIdx, fuzzyTodos ) =
    let
        viewTodo idx ( matchResult, todo ) =
            viewTodoListItem
                { selected = idx == maybeIdx
                , selectionIndicatorFocusMsg = SetSelectionIdx idx
                , todoId = todo.id
                , done = todo.done
                , doneChangedMsg = UpdateTodo todo << Todo.SetDone
                , noOpMsg = NoOp
                , title = todo.title
                , onClickRoot = FocusId <| selectionIndicatorDomId todo.id
                }
    in
    L.indexedMap viewTodo fuzzyTodos


selectionIndicatorDomId todoId =
    todoItemDomId todoId ++ "-si"


viewTodoListItem :
    { selected : Bool
    , selectionIndicatorFocusMsg : msg
    , todoId : String
    , done : Bool
    , doneChangedMsg : Bool -> msg
    , noOpMsg : msg
    , title : String
    , onClickRoot : msg
    }
    -> Element msg
viewTodoListItem vm =
    let
        { todoId, selected, done, doneChangedMsg, title, noOpMsg } =
            vm
    in
    r [ s1, fw, bwb 1, bc <| blackA 0.1, onClick vm.onClickRoot ]
        [ selectionIndicator selected (selectionIndicatorDomId todoId) vm.selectionIndicatorFocusMsg
        , r [ fw ]
            [ doneCheckBox done doneChangedMsg noOpMsg
            , displayTitle title
            ]
        ]


selectionIndicator selected domId focusMsg =
    el
        [ id domId |> fHA
        , ti_1
        , bwr 3
        , fh
        , bcIf selected blue100
        , focused [ bc blue400 ]
        , onFocus focusMsg
        ]
        (t "")


displayTitle title =
    el [ fw, p3 ] (t title)


doneCheckBox done doneChangedMsg noOpMsg =
    Input.checkbox
        [ p1
        , sw
        , brPill
        , fc grey500
        , focused [ Border.glow blue200 3, fc grey800 ]
        , mouseOver [ Border.glow blueGrey300 1, fc grey800 ]
        , onKeyDownPDBindAll
            [ ( HotKey.space, ( noOpMsg, True ) )
            ]
        ]
        { label = lh "done"
        , icon =
            \checked ->
                r [ fw, fh ]
                    [ ter checked Icons.checkCircleOutline Icons.circleOutline
                    ]
        , checked = done
        , onChange = doneChangedMsg
        }
