module UI exposing (appBar, viewLogo)

import El exposing (..)
import Element exposing (Element)


viewLogo =
    El.img
        [ El.fwx 24
        ]
        { src = "/logo.svg", description = "Logo" }


appBar : { title : String } -> Element msg
appBar { title } =
    el
        [ fw
        , bcBlack
        , fcWhite
        ]
        (r
            [ fwx 680
            , cx
            , p2
            , s2
            ]
            [ viewLogo
            , t title
            ]
        )
