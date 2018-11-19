module TodoLI exposing (Msg(..), displayTitle, doneCheckBox, selectionIndicator, view, xSelectionIndicator)

import BasicsX exposing (..)
import El exposing (..)
import Element as E exposing (Element, el, focused, mouseOver)
import Element.Border as Border
import Element.Events as EE exposing (onClick)
import Element.Input as EI
import EventX
import HotKey
import Html.Attributes exposing (class)
import Icons
import MaterialColorsUI exposing (..)
import Todo


type Msg
    = RootClicked
    | RootFocusInChanged Bool
    | Update Todo.Msg
    | PD


view :
    { selected : Bool
    , todoId : String
    , done : Bool
    , title : String
    }
    -> Element Msg
view vm =
    let
        { todoId, selected, done, title } =
            vm
    in
    r
        [ s1
        , fw
        , bwb 1
        , bc <| blackA 0.1
        , onClick RootClicked
        , fHA <| EventX.onFocusIn <| RootFocusInChanged True
        , fHA <| EventX.onFocusOut <| RootFocusInChanged False
        ]
        [ selectionIndicator selected vm
        , r [ fw ]
            [ doneCheckBox done
            , displayTitle title
            ]
        ]


xSelectionIndicator =
    "x-selection-indicator"


selectionIndicator selected vm =
    el
        [ fHA <| class xSelectionIndicator
        , ti_1
        , bwr 3
        , fh
        , bcIf selected blue400
        , onKeyDownPDBindAll
            [ ( HotKey.arrowDown, ( PD, True ) )
            , ( HotKey.arrowUp, ( PD, True ) )
            ]
        ]
        (t "")


displayTitle title =
    el [ fw, p3 ] (t title)


doneCheckBox done =
    EI.checkbox
        [ p1
        , sw
        , brPill
        , fc grey500
        , focused [ Border.glow blue200 3, fc grey800 ]
        , mouseOver [ Border.glow blueGrey300 1, fc grey800 ]
        , onKeyDownPDBindAll
            [ ( HotKey.space, ( PD, True ) )
            ]
        ]
        { label = lh "done"
        , icon =
            \checked ->
                r [ fw, fh ]
                    [ ter checked Icons.checkCircleOutline Icons.circleOutline
                    ]
        , checked = done
        , onChange = Update << Todo.SetDone
        }
