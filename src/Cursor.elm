module Cursor exposing
    ( Cursor
    , empty
    , indexOfSelectedIn
    , initAt
    , isAt
    , isAtNothing
    , rollBy
    , selected
    , selectionMap
    )

import Basics exposing ((|>))
import BasicsX exposing (..)
import List as L
import List.Extra as L
import Maybe as M
import Maybe.Extra as M


type alias Cursor =
    Maybe Int


empty : Cursor
empty =
    Nothing


initAt : Int -> Cursor
initAt idx =
    Just idx


isAt : Int -> Cursor -> Bool
isAt idx cursor =
    initAt idx == cursor


isAtNothing : Cursor -> Bool
isAtNothing cursor =
    cursor == empty


rollBy : Int -> List a -> Cursor -> Cursor
rollBy offset list =
    clampCursorIn list
        >> Maybe.map (add offset >> safeModBy (L.length list))


selected : List a -> Cursor -> Maybe a
selected list =
    clampCursorIn list >> M.andThen (flip L.getAt list)


indexOfSelectedIn : List a -> Cursor -> Maybe Int
indexOfSelectedIn =
    clampCursorIn


selectionMap fn list =
    let
        withSIdx sIdx =
            L.indexedMap (\idx -> fn idx <| sIdx == idx) list
    in
    clampCursorIn list
        >> M.unwrap [] withSIdx


clampCursorIn : List a -> Cursor -> Cursor
clampCursorIn list =
    M.withDefault 0 >> clampIdxIn list
