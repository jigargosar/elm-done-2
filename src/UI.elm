module UI exposing (appBar, title2, viewLogo)

import El exposing (..)
import Element exposing (Element)
import Element.Font


viewLogo =
    El.img
        [ El.fwx 24
        ]
        { src = "/logo.svg", description = "Logo" }


appBar : { title : Element msg } -> Element msg
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
            , title
            ]
        )


title2 w1 w2 =
    r [] [ el [ fxb ] (t w1), el [ fxl ] (t w2) ]
