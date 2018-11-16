module El exposing (bc, bcBlack, black, br, br2, c, cx, cy, eMap, eMapA, f, fb, fc, fcWhite, fh, fl, fromH, fromHA, fw, fwx, fxb, fxl, h, img, l, onEnterDown, onEscDown, onKeyDown, onKeyDownBindAll, p, p1, p2, p3, p4, pxy, r, s, s1, s2, s3, t, u1, u2, u3, u4, w, white)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import HotKey
import Html.Events
import Theme



-- shorthand


fromH =
    Element.html


fromHA =
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


fwx x =
    maximum x fill |> width


fh =
    height fill


s =
    spacing


bc =
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



-- Custom


black =
    Element.rgb 0 0 0


white =
    Element.rgb 1 1 1


bcBlack =
    bc black


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


onKeyDown decoder =
    Html.Events.on "keydown" decoder
        |> htmlAttribute


onEscDown =
    HotKey.onEscDown >> htmlAttribute


onEnterDown =
    HotKey.onEnterDown >> htmlAttribute


onKeyDownBindAll =
    HotKey.onKeyDownBindAll >> htmlAttribute
