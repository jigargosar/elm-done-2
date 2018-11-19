module TodoList exposing (Item(..), toFuzzyTodoList, todoSelectionList)

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

        filterMapFn todo =
            Fuzzy.match [] [] (boil query) (boil <| todo.title)
                |> initFuzzyValue todo
                |> justWhen (isScoreGreaterThan 1000)

        sort =
            L.sortBy getScore
    in
    L.filterMap filterMapFn



--        >> unlessBool (isBlank query) sort


todoSelectionList :
    { query : String, todoStore : TodoStore, selection : Selection }
    -> SelectionList Item
todoSelectionList { query, todoStore, selection } =
    let
        fuzzyTodoList =
            Todo.all todoStore
                |> toFuzzyTodoList query

        items =
            if isBlank query then
                fuzzyTodoList |> L.map FuzzyTodoLI

            else
                fuzzyTodoList
                    |> L.sortBy getScore
                    |> L.map FuzzyTodoLI
                    |> (::) (CreateTodoLI query)
    in
    selection |> SelectionList.withList items
