module EventX exposing (onKeyDown)

import Html.Events as HE


onKeyDown decoder =
    HE.on "keydown" decoder
