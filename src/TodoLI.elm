module TodoLI exposing
    ( FuzzyMsg(..)
    , FuzzyTodo
    , Item(..)
    , ItemMsg(..)
    , Msg(..)
    , TodoList
    , displayTitle
    , doneCheckBox
    , getFocusSelectorFor
    , initList
    , selectionIndicator
    , view
    , xSelectionIndicator
    )

import BasicsX exposing (..)
import El exposing (..)
import Element as E exposing (Element, el, focused, mouseOver)
import Element.Border as Border
import Element.Events as EE exposing (onClick)
import Element.Input as EI
import EventX
import Fuzzy
import HotKey
import Html.Attributes exposing (class)
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


itemDomId suffix =
    "todo-li--" ++ suffix


getFocusSelectorFor item =
    case item of
        FuzzyTodoLI { value } ->
            "#" ++ itemDomId value.id ++ " ." ++ xSelectionIndicator

        CreateTodoLI title ->
            "#" ++ itemDomId "create-todo-action" ++ xSelectionIndicator


type Msg
    = Msg Int ItemMsg


type ItemMsg
    = FuzzyChanged Todo FuzzyMsg
    | CreateTodoClicked


type FuzzyMsg
    = RootClicked
    | RootFocusInChanged Bool
    | Update Todo.Msg
    | PD


view : Int -> Bool -> Item -> Element Msg
view idx selected todoLi =
    E.map (Msg idx) <|
        case todoLi of
            FuzzyTodoLI li ->
                let
                    todo =
                        li.value
                in
                r
                    [ s1
                    , fw
                    , bwb 1
                    , bc <| blackA 0.1
                    , onClick RootClicked
                    , fHA <| EventX.onFocusIn <| RootFocusInChanged True
                    , fHA <| EventX.onFocusOut <| RootFocusInChanged False
                    ]
                    [ selectionIndicator selected
                    , r [ fw ]
                        [ doneCheckBox todo.done
                        , displayTitle todo.title
                        ]
                    ]
                    |> E.map (FuzzyChanged todo)

            CreateTodoLI title ->
                r
                    [ s3
                    , fw
                    , bwb 1
                    , bc <| blackA 0.1
                    , onClick CreateTodoClicked
                    ]
                    [ t "add task", t title ]


xSelectionIndicator =
    "x-selection-indicator"


selectionIndicator selected =
    el
        [ fHA <| class xSelectionIndicator
        , ti_1
        , bwr 3
        , fh
        , bcIf selected blue400
        , onKeyDownPDBindAll
            [ ( HotKey.arrowDown, ( PD, True ) )
            , ( HotKey.arrowUp, ( PD, True ) )
            ]
        ]
        (t "")


displayTitle title =
    el [ fw, p3 ] (t title)


doneCheckBox done =
    EI.checkbox
        [ p1
        , sw
        , brPill
        , fc grey500
        , focused [ Border.glow blue200 3, fc grey800 ]
        , mouseOver [ Border.glow blueGrey300 1, fc grey800 ]
        , onKeyDownPDBindAll
            [ ( HotKey.space, ( PD, True ) )
            ]
        ]
        { label = lh "done"
        , icon =
            \checked ->
                r [ fw, fh ]
                    [ ter checked Icons.checkCircleOutline Icons.circleOutline
                    ]
        , checked = done
        , onChange = Update << Todo.SetDone
        }
