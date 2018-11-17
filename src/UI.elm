module UI exposing (appBar, layout, logo, rootLayer, title2)

import El exposing (..)
import Element exposing (..)
import Element.Font
import Theme


rootLayer attrs =
    l
        ([ Theme.baseFontFamily
         , Theme.baseFontSize
         ]
            ++ attrs
        )
        (t "")


layout : { appBar : Element msg, content : Element msg } -> Element msg
layout config =
    c [ fw, clip, scrollbarY ]
        [ config.appBar
        , config.content
        ]


logo =
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
            [ fwx Theme.maxWidth
            , cx
            , p2
            , s2
            ]
            [ logo
            , title
            ]
        )


title2 w1 w2 =
    r [] [ el [ fxb ] (t w1), el [ fxl ] (t w2) ]
