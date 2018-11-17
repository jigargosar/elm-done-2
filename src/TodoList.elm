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
import Element exposing (Element, clip, el, fromRgb, fromRgb255, rgb, rgba, scrollbarY)
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
import MaterialColorsElmUI exposing (..)
import Theme
import Todo
import TodoStore as TS
import UpdateX exposing (..)


type alias ModelRecord =
    { inputText : String
    , todoStore : TS.Model
    , selectedIdx : Maybe Int
    , inputHasFocus : Bool
    }


type Model
    = Model ModelRecord


unwrap (Model model) =
    model


inputText =
    unwrap >> .inputText


todoStore =
    unwrap >> .todoStore


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


currentList (Model model) =
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


setSelectedIdxIn (Model model) idx =
    Model { model | selectedIdx = Just idx }


setInputHasFocus val (Model model) =
    Model { model | inputHasFocus = val }


updateSelectedIdxBy numFn model =
    currentList model
        |> unwrapMaybe model
            (Tuple.mapBoth
                numFn
                (List.length >> (+) -1)
                >> (\( idx, idxMax ) -> safeModBy idxMax idx)
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
    | LoadTS Value
    | NoOp


empty : Model
empty =
    Model
        { inputText = ""
        , todoStore = TS.empty
        , selectedIdx = Nothing
        , inputHasFocus = False
        }


setInputText val (Model model) =
    Model { model | inputText = val }


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
            andThenF (\model -> updateTS (TS.new (inputText model) ""))
                >> mapModel (setInputText "")

        TSMsg msg ->
            updateTS msg

        LoadTS value ->
            updateTS (TS.Load value)

        NoOp ->
            identity


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
        , text = inputText model
        , placeholder = ipp [] (t "Add... / Search...")
        , label = lh "Task Title"
        }


viewTodo si idx ( matchResult, todo ) =
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

        doneCheckBox =
            Input.checkbox [ pxy u3 u2, sw ]
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
            el [ pxy u3 u2 ] (t <| Todo.title todo)
    in
    r
        [ fw
        , bgc blue50
        ]
        [ selectionIndicator
        , doneCheckBox
        , displayTitle
        ]
