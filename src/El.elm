module El exposing
    ( a0
    , bc
    , bcBlack
    , bcIf
    , bgc
    , black
    , blackA
    , br
    , br2
    , brPill
    , bw
    , bwb
    , bwr
    , c
    , cx
    , cy
    , eMap
    , eMapA
    , f
    , fHA
    , fb
    , fc
    , fcWhite
    , fh
    , fl
    , fromH
    , fw
    , fwx
    , fxb
    , fxl
    , h
    , img
    , ip
    , ipp
    , l
    , la
    , lh
    , onKeyDown
    , onKeyDownBindAll
    , onKeyDownPD
    , onKeyDownPDBindAll
    , p
    , p1
    , p2
    , p3
    , p4
    , pxy
    , r
    , s
    , s1
    , s2
    , s3
    , sw
    , t
    , ti
    , ti0
    , ti_1
    , u1
    , u2
    , u3
    , u4
    , w
    , white
    )

import BasicsX exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import EventX
import HotKey
import Html.Attributes exposing (tabindex)
import Html.Events
import Theme



-- shorthand


fromH =
    Element.html


fHA =
    Element.htmlAttribute


eMap =
    Element.map


eMapA =
    Element.mapAttribute


img =
    image


r =
    row


c =
    column


l =
    layout


cx =
    centerX


cy =
    centerY


t =
    text


w =
    width


h =
    height


p =
    padding


pxy =
    paddingXY


f =
    fill


fw =
    width fill


sw =
    width shrink


fwx x =
    maximum x fill |> width


fh =
    height fill


s =
    spacing


bgc =
    Background.color


fc =
    Font.color


fl =
    Font.light


fxl =
    Font.extraLight


fxb =
    Font.extraBold


fb =
    Font.bold


br =
    Border.rounded


brPill =
    br 9999


bw =
    Border.width


each x =
    { bottom = x, top = x, left = x, right = x }


each0 =
    each 0


bwb x =
    Border.widthEach { each0 | bottom = x }


bwr x =
    Border.widthEach { each0 | right = x }


bc =
    Border.color


bcIf bool clr =
    bc <| ter bool clr a0



-- Input short hand


ip =
    Input.text


type alias Attributes msg =
    List (Element.Attribute msg)


ipp : Attributes msg -> Element msg -> Maybe (Input.Placeholder msg)
ipp attrs e =
    Just <| Input.placeholder attrs e


la =
    Input.labelAbove


lh =
    Input.labelHidden



-- Custom


a0 =
    rgba 0 0 0 0


black =
    rgb 0 0 0


blackA x =
    rgba 0 0 0 x


white =
    rgb 1 1 1


bcBlack =
    bgc black


fcWhite =
    fc white


u1 =
    4


u2 =
    8


u3 =
    12


u4 =
    16


s1 =
    s u1


s2 =
    s u2


s3 =
    s u3


p1 =
    p u1


p2 =
    p u2


p3 =
    p u3


p4 =
    p u4


br2 =
    br u2



-- Html Attributes


ti =
    tabindex >> fHA


ti0 =
    ti 0


ti_1 =
    ti -1


onKeyDown =
    EventX.onKeyDown >> fHA


onKeyDownPD =
    EventX.onKeyDownPD >> fHA


onKeyDownPDBindAll =
    onKeyDownPD << HotKey.bindAll


onKeyDownBindAll =
    onKeyDown << HotKey.bindAll
