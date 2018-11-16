module Theme exposing (baseFontFamily, baseFontSize, maxWidth)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


baseFontFamily =
    [ "-apple-system"
    , "BlinkMacSystemFont"
    , "avenir next"
    , "avenir"
    , "helvetica neue"
    , "helvetica"
    , "ubuntu"
    , "roboto"
    , "noto"
    , "segoe ui"
    , "arial"
    , "sans-serif"
    ]
        |> List.map Font.typeface
        |> Font.family


baseFontSize =
    Font.size 16


maxWidth =
    480
