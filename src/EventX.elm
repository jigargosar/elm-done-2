module EventX exposing (onFocusIn, onFocusOut, onKeyDown, onKeyDownPD)

import Html.Events as HE
import Json.Decode as D
import Json.Encode as E


onKeyDown decoder =
    HE.on "keydown" decoder


onKeyDownPD =
    HE.preventDefaultOn "keydown"


onFocusIn tagger =
    HE.on "focusin" (D.succeed tagger)


onFocusOut tagger =
    HE.on "focusout" (D.succeed tagger)
