module UI exposing (appBar, viewLogo)

import El exposing (..)


viewLogo =
    El.img
        [ El.fwx 24
        ]
        { src = "/logo.svg", description = "Logo" }


appBar =
    e
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
            , t "ELM Done 2"
            ]
        )
