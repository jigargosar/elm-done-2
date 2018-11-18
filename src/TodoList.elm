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
import Todo exposing (Todo)
import TodoStore as TS
import UpdateX exposing (..)


type alias Model =
    { inputText : String
    , todoStore : TS.TodoStore
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


currentList model =
    let
        filteredList =
            TS.all model.todoStore
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


setSelectedIdxIn model idx =
    { model | selectedIdx = Just idx }


setInputHasFocus val model =
    { model | inputHasFocus = val }


updateSelectedIdxBy numFn model =
    currentList model
        |> unwrapMaybe model
            (Tuple.mapBoth
                numFn
                List.length
                >> (\( idx, length ) -> safeModBy length idx)
                >> setSelectedIdxIn model
            )


type Msg
    = ---- INJECT MSG BELOW ----
      OnPrev
    | OnNext
    | OnDoneChanged Todo Bool
    | InputChanged String
    | InputFocusChanged Bool
    | Submit
    | NewTodo TS.TodoBuilder
    | LoadTS Value
    | NoOp


empty : Model
empty =
    { inputText = ""
    , todoStore = TS.empty
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

        OnDoneChanged todo bool ->
            updateTS (TS.modTodo (Todo.SetDone bool) todo) model

        InputChanged value ->
            pure <| setInputText value model

        InputFocusChanged hasFocus ->
            pure <| setInputHasFocus hasFocus model

        Submit ->
            onNewTodoMsg (TS.initBuilder model.inputText "") model
                |> mapModel (setInputText "")

        NewTodo builder ->
            onNewTodoMsg builder model

        LoadTS value ->
            updateTS (\_ -> TS.restore value) model

        NoOp ->
            pure model


type alias ReturnF =
    ( Model, Cmd Msg ) -> ( Model, Cmd Msg )


updateTS fn model =
    fn model.todoStore
        |> Tuple.mapFirst (\ts -> { model | todoStore = ts })


onNewTodoMsg : TS.TodoBuilder -> Model -> ( Model, Cmd Msg )
onNewTodoMsg =
    updateTS << TS.new NewTodo


view : Model -> Element Msg
view model =
    c [ fw, clip, scrollbarY ]
        [ el [ p3, cx, fwx Theme.maxWidth ] <| viewInput model
        , el [ fw, clip, scrollbarY ] <|
            c [ cx, fwx Theme.maxWidth ] <|
                case currentList model of
                    Nothing ->
                        [ t "No Tasks Found" ]

                    Just ( si, todos ) ->
                        List.indexedMap (viewTodo si) todos
        ]


viewInput model =
    ip
        [ br2
        , p2
        , onLoseFocus <| InputFocusChanged False
        , onFocus <| InputFocusChanged True
        , onKeyDownPDBindAll
            [ ( HotKey.arrowDown, ( NoOp, True ) )
            , ( HotKey.arrowUp, ( NoOp, True ) )
            , ( HotKey.enter, ( Submit, False ) )
            ]
        ]
        { onChange = InputChanged
        , text = model.inputText
        , placeholder = ipp [] (t "Add... / Search...")
        , label = lh "Task Title"
        }


viewTodo selectedIdx idx ( matchResult, todo ) =
    viewTodoListItem
        { selected = idx == selectedIdx
        , done = todo.done
        , title = todo.title
        , doneChangedMsg = OnDoneChanged todo
        , noOpMsg = NoOp
        }


viewTodoListItem { selected, done, doneChangedMsg, title, noOpMsg } =
    let
        selectionIndicator =
            el
                [ bwr 3
                , fh
                , bcIf selected blue400
                ]
                (t "")

        doneCheckBox =
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

        displayTitle =
            el [ fw, p3 ] (t title)

        listRow =
            r [ s1, fw, bwb 1, bc <| blackA 0.1 ]

        listInnerRow =
            r [ fw ]
    in
    listRow
        [ selectionIndicator
        , listInnerRow
            [ doneCheckBox
            , displayTitle
            ]
        ]
