module Cursor exposing (Cursor, empty, indexOfSelectedIn, initAt, isAt, isAtNothing, rollBy, selected)

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
    if L.isEmpty list then
        always Nothing
        --        identity

    else
        M.map
            (clamp 0 (L.length list - 1)
                >> (+) offset
                >> safeModBy (List.length list)
            )


selected : List a -> Cursor -> Maybe a
selected list =
    M.andThen (clampIdxIn list)
        >> M.andThen (flip L.getAt list)


indexOfSelectedIn list =
    M.andThen (clampIdxIn list)