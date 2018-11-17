module EventX exposing (onKeyDown, onKeyDownPD)

import Html.Events as HE


onKeyDown decoder =
    HE.on "keydown" decoder


onKeyDownPD =
    HE.preventDefaultOn "keydown"
