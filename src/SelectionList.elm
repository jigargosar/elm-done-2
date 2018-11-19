module SelectionList exposing (Selection, SelectionList, empty, getSelectedItem, rollBy, selectionMap, toList, toSelection, withList)

import Basics exposing ((|>))
import BasicsX exposing (..)
import List as L
import List.Extra as L
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


clampSelectedIdx : List a -> Selection -> Selection
clampSelectedIdx list =
    M.withDefault 0 >> clampIdx list


cycleSelectedIdx : List a -> Selection -> Selection
cycleSelectedIdx list =
    M.map (safeModBy (List.length list))


cycleIdx offset list maybeIdx =
    clampSelectedIdx list maybeIdx
        |> M.map ((+) offset)
        |> cycleSelectedIdx list


rollBy offset (SelectionList list maybeSelectionIdx) =
    SelectionList list (cycleIdx offset list maybeSelectionIdx)


toSelection (SelectionList list maybeIdx) =
    maybeIdx


getSelectedItem : SelectionList a -> Maybe a
getSelectedItem (SelectionList list maybeSelectionIdx) =
    clampSelectedIdx list maybeSelectionIdx |> Maybe.andThen (\idx -> L.getAt idx list)
