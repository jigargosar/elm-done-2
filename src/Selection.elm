module Selection exposing (Selection(..), SelectionList(..), none)


type Selection a
    = Selection Int (Maybe a)


type SelectionList a
    = SelectionList (List a) (Selection a)


none =
    Selection -1 Nothing
