module UpdateX exposing (addCmd, andThen, andThenF, effect, mapModel, pure)


pure m =
    ( m, Cmd.none )


effect f ( m, c ) =
    ( m, c ) |> addCmd (f m)


addCmd c2 ( m, c ) =
    ( m, Cmd.batch [ c, c2 ] )


mapModel =
    Tuple.mapFirst


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ] )


andThenF f =
    andThen (\m -> pure m |> andThen (f m))
