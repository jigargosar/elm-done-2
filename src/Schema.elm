module Schema exposing (Schema)


type alias Schema big =
    List (Property big)


type Property big
    = Text (TextProperty big)
    | Bool (BoolProperty big)


type alias TextProperty big =
    { get : big -> String, set : String -> big -> big }


type alias BoolProperty big =
    { get : big -> Bool, set : Bool -> big -> big }


type alias TodoRecord =
    { title : String, done : Bool }


type alias TodoSchema =
    Schema TodoRecord
