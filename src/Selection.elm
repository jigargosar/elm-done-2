module Selection exposing (Selection, SelectionList, empty, selectionMap, toList, withList)


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
                maybeIdx |> Maybe.withDefault 0
        in
        List.indexedMap (\idx -> fn idx <| sIdx == idx) list


toList (SelectionList list maybeIdx) =
    list
