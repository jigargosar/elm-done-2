module TodoList exposing (Item(..), TodoList, init)

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



--        >> unlessBool (isBlank query) sort


type alias TodoList =
    SelectionList Item


init :
    { query : String, todoStore : TodoStore, selection : Selection }
    -> SelectionList Item
init { query, todoStore, selection } =
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
