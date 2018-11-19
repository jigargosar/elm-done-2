module Selection exposing (Selection, SelectionList, empty, withList)


type alias Selection =
    Maybe Int


type SelectionList a
    = SelectionList (List a) Selection


empty =
    Nothing


withList l s =
    SelectionList l s
