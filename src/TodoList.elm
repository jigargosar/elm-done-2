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
import Todo
import TodoStore as TS
import UpdateX exposing (..)


type alias Model =
    { inputText : String
    , todoStore : TS.Model
    , selectedIdx : Maybe Int
    , inputHasFocus : Bool
    }


fuzzySort query =
    let
        boil =
            String.toLower

        fuzzMatcher todo =
            Fuzzy.match [] [] (boil query) (boil <| Todo.title todo)
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
                |> fuzzySort model.inputText
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
    | OnDoneChanged Todo.Model Bool
    | InputChanged String
    | InputFocusChanged Bool
    | Submit
    | TSMsg TS.Msg
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
update message =
    updateF message << pure


type alias ReturnF =
    ( Model, Cmd Msg ) -> ( Model, Cmd Msg )


updateF : Msg -> ReturnF
updateF message =
    case message of
        ---- INJECT UPDATE CASE BELOW ----
        OnPrev ->
            mapModel <| updateSelectedIdxBy ((+) -1)

        OnNext ->
            mapModel <| updateSelectedIdxBy ((+) 1)

        OnDoneChanged todo bool ->
            updateTS <| TS.ModTodo (Todo.SetDone bool) todo

        InputChanged value ->
            mapModel <| setInputText value

        InputFocusChanged hasFocus ->
            mapModel <| setInputHasFocus hasFocus

        Submit ->
            andThenF (\model -> onNewTodoMsg <| TS.initBuilder model.inputText "")
                >> mapModel (setInputText "")

        TSMsg msg ->
            updateTS msg

        NewTodo builder ->
            onNewTodoMsg builder

        LoadTS value ->
            andThen
                (\model ->
                    Tuple.mapBoth
                        (\todoStore ->
                            { model | todoStore = todoStore }
                        )
                        (Cmd.map TSMsg)
                        (TS.restore value)
                )

        NoOp ->
            identity


onNewTodoMsg : TS.TodoBuilder -> ReturnF
onNewTodoMsg builder =
    andThen <|
        \model ->
            Tuple.mapBoth
                (\todoStore ->
                    { model | todoStore = todoStore }
                )
                identity
                (TS.new NewTodo builder model.todoStore)


updateTS message =
    andThen
        (\model ->
            let
                ( todoStore_, cmd ) =
                    (case message of
                        TS.ModTodo msg todo ->
                            TS.onModTodoMsg msg todo
                    )
                    <|
                        pure model.todoStore
            in
            ( { model | todoStore = todoStore_ }, Cmd.map TSMsg cmd )
        )


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
        , onKeyDownPD <|
            HotKey.bindAll
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


viewTodo si idx ( matchResult, todo ) =
    let
        isSelected =
            idx == si

        selectionIndicator =
            el
                [ bwr 3
                , fh
                , bcIf isSelected blue400
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
                , onKeyDownPD <|
                    HotKey.bindAll
                        [ ( HotKey.space, ( NoOp, True ) )
                        ]
                ]
                { label = lh "done"
                , icon =
                    \checked ->
                        r [ fw, fh ]
                            [ ter checked Icons.checkCircleOutline Icons.circleOutline
                            ]
                , checked = Todo.done todo
                , onChange = OnDoneChanged todo
                }

        displayTitle =
            el [ fw, p3 ] (t <| Todo.title todo)

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
