module Icons exposing (trash)

import Element exposing (html)
import Html.Attributes exposing (attribute)
import Svg exposing (svg)
import Svg.Attributes exposing (..)


trash =
    Svg.svg
        [ attribute "height" "24"
        , viewBox "0 0 24 24"
        , attribute "width" "24"
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ Svg.path [ d "M6 19c0 1.1.9 2 2 2h8c1.1 0 2-.9 2-2V7H6v12zM19 4h-3.5l-1-1h-5l-1 1H5v2h14V4z" ]
            []
        , Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        ]
        |> html
