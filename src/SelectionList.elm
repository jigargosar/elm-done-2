module SelectionList exposing (Selection, SelectionList, circleSelectionByOffset, empty, selectionMap, toList, toSelection, withList)

import Basics exposing ((|>))
import BasicsX exposing (..)
import Maybe as M
import Maybe.Extra as M


type alias Selection =
    Maybe Int


type SelectionList a
    = SelectionList (List a) Selection


empty =
    Nothing


withList l s =
    SelectionList l s


selectionMap fn (SelectionList list maybeIdx) =
    if List.isEmpty list then
        []

    else
        let
            sIdx =
                maybeIdx |> M.withDefault 0
        in
        List.indexedMap (\idx -> fn idx <| sIdx == idx) list


toList (SelectionList list maybeIdx) =
    list


getSelectedIndexForList : List a -> Maybe Int -> Int
getSelectedIndexForList list =
    M.andThen (clampIdx list) >> M.withDefault 0


circleSelectionByOffset offset (SelectionList list maybeIdx) =
    if List.isEmpty list then
        SelectionList list maybeIdx

    else
        let
            sIdx =
                getSelectedIndexForList list maybeIdx
        in
        SelectionList list maybeIdx


toSelection (SelectionList list maybeIdx) =
    maybeIdx
