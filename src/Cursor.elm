module Cursor exposing (Cursor, at, atNothing, isAt, isAtNothing, rollBy)

import Basics exposing ((|>))
import BasicsX exposing (..)
import List as L
import List.Extra as L
import Maybe as M
import Maybe.Extra as M


type alias Cursor =
    Maybe Int


atNothing : Cursor
atNothing =
    Nothing


at : Int -> Cursor
at idx =
    Just idx


isAt : Int -> Cursor -> Bool
isAt idx cursor =
    at idx == cursor


isAtNothing : Cursor -> Bool
isAtNothing cursor =
    cursor == atNothing


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
