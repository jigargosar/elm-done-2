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
import Element exposing (Element, clip, el, focused, fromRgb, fromRgb255, mouseOver, rgb, rgba, scrollbarY)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onFocus, onLoseFocus)
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Element.Region as Region
import Fuzzy
import HotKey
import Html.Attributes exposing (tabindex)
import Html.Events
import Icons
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MaterialColorsUI exposing (..)
import Theme
import Todo exposing (Todo, TodoStore)
import UpdateX exposing (..)


type alias Model =
    { inputText : String
    , todoStore : TodoStore
    , selectedIdx : Maybe Int
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
            List.sortBy (Tuple.first >> .score)
    in
    List.filterMap filterMapFn
        >> unlessBool (isBlank query) sort


maybeTodoListViewModel : Model -> Maybe ( Int, List ( Fuzzy.Result, Todo ) )
maybeTodoListViewModel model =
    let
        filteredList =
            Todo.all model.todoStore
                |> filterWithFuzzyResult model.inputText
    in
    if List.isEmpty filteredList then
        Nothing

    else
        let
            idxMax =
                List.length filteredList - 1
        in
        model.selectedIdx |> unwrapMaybe 0 (min idxMax) |> (\idx -> Just ( idx, filteredList ))


updateSelectedIdxBy numFn model =
    maybeTodoListViewModel model
        |> unwrapMaybe model
            (Tuple.mapBoth
                numFn
                List.length
                >> (\( idx, length ) -> safeModBy length idx)
                >> (\idx -> { model | selectedIdx = Just idx })
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
    | Submit
    | NewTodo Todo.TodoBuilder
    | LoadTS Value
    | NoOp


empty : Model
empty =
    { inputText = ""
    , todoStore = Todo.emptyStore
    , selectedIdx = Nothing
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
            pure <| updateSelectedIdxBy ((+) -1) model

        OnNext ->
            pure <| updateSelectedIdxBy ((+) 1) model

        UpdateTodo todo msg ->
            updateTS (Todo.update msg todo) model

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


viewTodoList ( selectedIdx, fuzzyTodos ) =
    let
        viewTodo idx ( matchResult, todo ) =
            viewTodoListItem
                { selected = idx == selectedIdx
                , done = todo.done
                , title = todo.title
                , doneChangedMsg = UpdateTodo todo << Todo.SetDone
                , noOpMsg = NoOp
                }
    in
    List.indexedMap viewTodo fuzzyTodos


viewTodoListItem :
    { selected : Bool
    , done : Bool
    , doneChangedMsg : Bool -> msg
    , title : String
    , noOpMsg : msg
    }
    -> Element msg
viewTodoListItem viewModel =
    let
        { selected, done, doneChangedMsg, title, noOpMsg } =
            viewModel

        listRow =
            r [ s1, fw, bwb 1, bc <| blackA 0.1 ]

        listInnerRow =
            r [ fw ]
    in
    listRow
        [ selectionIndicator selected
        , listInnerRow
            [ doneCheckBox done doneChangedMsg noOpMsg
            , displayTitle title
            ]
        ]


selectionIndicator selected =
    el
        [ bwr 3
        , fh
        , bcIf selected blue400
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
