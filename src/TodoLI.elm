module TodoLI exposing
    ( FuzzyTodo
    , Item(..)
    , ItemMsg(..)
    , Msg
    , TodoList
    , displayTitle
    , doneCheckBox
    , getFocusSelectorForItem
    , initList
    , selectionIndicator
    , view
    , xSelectionIndicator
    )

import BasicsX exposing (..)
import El exposing (..)
import Element as E exposing (Attribute, Element, el, focused, mouseOver)
import Element.Border as Border
import Element.Events as EE exposing (onClick)
import Element.Input as EI
import EventX
import Fuzzy
import HotKey as HK
import Html.Attributes exposing (class, id)
import Icons
import List as L
import List.Extra as L
import MaterialColorsUI exposing (..)
import Maybe as M
import Maybe.Extra as M
import SelectionList exposing (Selection, SelectionList)
import Todo exposing (Todo, TodoStore)


type alias FuzzyValue a =
    { value : a, result : Fuzzy.Result }


initFuzzyValue : a -> Fuzzy.Result -> FuzzyValue a
initFuzzyValue a result =
    { value = a, result = result }


getScore : FuzzyValue a -> Int
getScore =
    .result >> .score


isScoreGreaterThan num { result } =
    result.score > num


isScoreLessThan num { result } =
    result.score < num


type alias FuzzyTodo =
    FuzzyValue Todo


type Item
    = FuzzyTodoLI FuzzyTodo
    | CreateTodoLI String



--    | InputField String


toFuzzyTodoList : String -> List Todo -> List FuzzyTodo
toFuzzyTodoList query =
    let
        boil =
            String.toLower

        filterFn todo =
            Fuzzy.match [] [] (boil query) (boil <| todo.title)
                |> initFuzzyValue todo
    in
    L.map filterFn


type alias TodoList =
    SelectionList Item


initList :
    { query : String, todoStore : TodoStore, selection : Selection }
    -> SelectionList Item
initList { query, todoStore, selection } =
    let
        fuzzyTodoList =
            Todo.all todoStore
                |> toFuzzyTodoList query

        items =
            if isBlank query then
                fuzzyTodoList |> L.map FuzzyTodoLI

            else
                fuzzyTodoList
                    |> L.filter (isScoreLessThan 1000)
                    |> L.sortBy getScore
                    |> L.map FuzzyTodoLI
                    |> (::) (CreateTodoLI query)
    in
    selection |> SelectionList.withList items


itemDomIdPrefix =
    "todo-li--"


getItemDomId item =
    itemDomIdPrefix
        ++ (case item of
                FuzzyTodoLI { value } ->
                    value.id

                CreateTodoLI title ->
                    "create-todo-action"
           )


getFocusSelectorForItem item =
    let
        itemDomId =
            getItemDomId item
    in
    "#" ++ itemDomId ++ " ." ++ xSelectionIndicator


type alias Msg =
    { idx : Int, itemMsg : ItemMsg }


type ItemMsg
    = Update Todo Todo.Msg
    | RootClicked
    | Create
    | RootFocusInChanged Bool
    | PD


view : { selectionHasFocus : Bool } -> Int -> Bool -> Item -> Element Msg
view config idx selected item =
    let
        rootEl : List (Attribute ItemMsg) -> List (Element ItemMsg) -> Element ItemMsg
        rootEl attrs =
            r <|
                ([ fHA <| id <| getItemDomId item
                 , s1
                 , fw
                 , fh
                 , bwb 1
                 , bc <| blackA 0.1
                 , fHA <| EventX.onFocusIn <| RootFocusInChanged True
                 , fHA <| EventX.onFocusOut <| RootFocusInChanged False
                 ]
                    ++ attrs
                )

        rootView =
            case item of
                FuzzyTodoLI fuzzyTodo ->
                    let
                        todo =
                            fuzzyTodo.value
                    in
                    rootEl [ onClick RootClicked ]
                        [ selectionIndicator config selected
                        , r [ fw ]
                            [ doneCheckBox todo
                            , displayTitle todo.title
                            ]
                        ]

                CreateTodoLI title ->
                    rootEl [ onClick Create ]
                        [ selectionIndicator config selected
                        , displayTitle " + add task"
                        ]
    in
    rootView
        |> E.map (Msg idx)


xSelectionIndicator =
    "x-selection-indicator"


selectionIndicator { selectionHasFocus } selected =
    el
        [ fHA <| class xSelectionIndicator
        , ti_1
        , bwr 3
        , fh
        , bc <|
            if selected && selectionHasFocus then
                blue400

            else if selected then
                blue100

            else
                a0
        , onKeyDownPD <|
            HK.bindEachToMsg
                [ ( HK.arrowDown, ( PD, True ) )
                , ( HK.arrowUp, ( PD, True ) )
                ]
        ]
        (t "")


displayTitle title =
    el [ fw, p3, ti_1 ] (t title)


doneCheckBox todo =
    EI.checkbox
        [ p1
        , sw
        , brPill
        , fc grey500
        , focused [ Border.glow blue200 3, fc grey800 ]
        , mouseOver [ Border.glow blueGrey300 1, fc grey800 ]
        , onKeyDownPD <|
            HK.bindEachToMsg
                [ ( HK.space, ( PD, True ) )
                ]
        ]
        { label = lh "done"
        , icon =
            \checked ->
                r [ fw, fh ]
                    [ ter checked Icons.checkCircleOutline Icons.circleOutline
                    ]
        , checked = todo.done
        , onChange = Update todo << Todo.SetDone
        }
