module MaterialColorsElmUI exposing
    ( red50, red100, red200, red300, red400, red500, red600, red700, red800, red900, redA100, redA200, redA400, redA700
    , pink50, pink100, pink200, pink300, pink400, pink500, pink600, pink700, pink800, pink900, pinkA100, pinkA200, pinkA400, pinkA700
    , purple50, purple100, purple200, purple300, purple400, purple500, purple600, purple700, purple800, purple900, purpleA100, purpleA200, purpleA400, purpleA700
    , deepPurple50, deepPurple100, deepPurple200, deepPurple300, deepPurple400, deepPurple500, deepPurple600, deepPurple700, deepPurple800, deepPurple900, deepPurpleA100, deepPurpleA200, deepPurpleA400, deepPurpleA700
    , indigo50, indigo100, indigo200, indigo300, indigo400, indigo500, indigo600, indigo700, indigo800, indigo900, indigoA100, indigoA200, indigoA400, indigoA700
    , blue50, blue100, blue200, blue300, blue400, blue500, blue600, blue700, blue800, blue900, blueA100, blueA200, blueA400, blueA700
    , lightBlue50, lightBlue100, lightBlue200, lightBlue300, lightBlue400, lightBlue500, lightBlue600, lightBlue700, lightBlue800, lightBlue900, lightBlueA100, lightBlueA200, lightBlueA400, lightBlueA700
    , cyan50, cyan100, cyan200, cyan300, cyan400, cyan500, cyan600, cyan700, cyan800, cyan900, cyanA100, cyanA200, cyanA400, cyanA700
    , teal50, teal100, teal200, teal300, teal400, teal500, teal600, teal700, teal800, teal900, tealA100, tealA200, tealA400, tealA700
    , green50, green100, green200, green300, green400, green500, green600, green700, green800, green900, greenA100, greenA200, greenA400, greenA700
    , lightGreen50, lightGreen100, lightGreen200, lightGreen300, lightGreen400, lightGreen500, lightGreen600, lightGreen700, lightGreen800, lightGreen900, lightGreenA100, lightGreenA200, lightGreenA400, lightGreenA700
    , lime50, lime100, lime200, lime300, lime400, lime500, lime600, lime700, lime800, lime900, limeA100, limeA200, limeA400, limeA700
    , yellow50, yellow100, yellow200, yellow300, yellow400, yellow500, yellow600, yellow700, yellow800, yellow900, yellowA100, yellowA200, yellowA400, yellowA700
    , amber50, amber100, amber200, amber300, amber400, amber500, amber600, amber700, amber800, amber900, amberA100, amberA200, amberA400, amberA700
    , orange50, orange100, orange200, orange300, orange400, orange500, orange600, orange700, orange800, orange900, orangeA100, orangeA200, orangeA400, orangeA700
    , deepOrange50, deepOrange100, deepOrange200, deepOrange300, deepOrange400, deepOrange500, deepOrange600, deepOrange700, deepOrange800, deepOrange900, deepOrangeA100, deepOrangeA200, deepOrangeA400, deepOrangeA700
    , brown50, brown100, brown200, brown300, brown400, brown500, brown600, brown700, brown800, brown900
    , grey50, grey100, grey200, grey300, grey400, grey500, grey600, grey700, grey800, grey900
    , blueGrey50, blueGrey100, blueGrey200, blueGrey300, blueGrey400, blueGrey500, blueGrey600, blueGrey700, blueGrey800, blueGrey900
    , Rgba255
    )

{-| This library allow to get Material colors as Elm


# Helpers

@docs Rgb255, rgb255, toRgbString


# Colors


## Red Hue

@docs red50, red100, red200, red300, red400, red500, red600, red700, red800, red900, redA100, redA200, redA400, redA700


## Pink Hue

@docs pink50, pink100, pink200, pink300, pink400, pink500, pink600, pink700, pink800, pink900, pinkA100, pinkA200, pinkA400, pinkA700


## Purple Hue

@docs purple50, purple100, purple200, purple300, purple400, purple500, purple600, purple700, purple800, purple900, purpleA100, purpleA200, purpleA400, purpleA700


## Deep Purple Hue

@docs deepPurple50, deepPurple100, deepPurple200, deepPurple300, deepPurple400, deepPurple500, deepPurple600, deepPurple700, deepPurple800, deepPurple900, deepPurpleA100, deepPurpleA200, deepPurpleA400, deepPurpleA700


## Indigo Hue

@docs indigo50, indigo100, indigo200, indigo300, indigo400, indigo500, indigo600, indigo700, indigo800, indigo900, indigoA100, indigoA200, indigoA400, indigoA700


## Blue Hue

@docs blue50, blue100, blue200, blue300, blue400, blue500, blue600, blue700, blue800, blue900, blueA100, blueA200, blueA400, blueA700


## Light Blue Hue

@docs lightBlue50, lightBlue100, lightBlue200, lightBlue300, lightBlue400, lightBlue500, lightBlue600, lightBlue700, lightBlue800, lightBlue900, lightBlueA100, lightBlueA200, lightBlueA400, lightBlueA700


## Cyan Hue

@docs cyan50, cyan100, cyan200, cyan300, cyan400, cyan500, cyan600, cyan700, cyan800, cyan900, cyanA100, cyanA200, cyanA400, cyanA700


## Teal Hue

@docs teal50, teal100, teal200, teal300, teal400, teal500, teal600, teal700, teal800, teal900, tealA100, tealA200, tealA400, tealA700


## Green Hue

@docs green50, green100, green200, green300, green400, green500, green600, green700, green800, green900, greenA100, greenA200, greenA400, greenA700


## Light Green Hue

@docs lightGreen50, lightGreen100, lightGreen200, lightGreen300, lightGreen400, lightGreen500, lightGreen600, lightGreen700, lightGreen800, lightGreen900, lightGreenA100, lightGreenA200, lightGreenA400, lightGreenA700


## Lime Hue

@docs lime50, lime100, lime200, lime300, lime400, lime500, lime600, lime700, lime800, lime900, limeA100, limeA200, limeA400, limeA700


## Yellow Hue

@docs yellow50, yellow100, yellow200, yellow300, yellow400, yellow500, yellow600, yellow700, yellow800, yellow900, yellowA100, yellowA200, yellowA400, yellowA700


## Amber Hue

@docs amber50, amber100, amber200, amber300, amber400, amber500, amber600, amber700, amber800, amber900, amberA100, amberA200, amberA400, amberA700


## Orange Hue

@docs orange50, orange100, orange200, orange300, orange400, orange500, orange600, orange700, orange800, orange900, orangeA100, orangeA200, orangeA400, orangeA700


## Deep Orange Hue

@docs deepOrange50, deepOrange100, deepOrange200, deepOrange300, deepOrange400, deepOrange500, deepOrange600, deepOrange700, deepOrange800, deepOrange900, deepOrangeA100, deepOrangeA200, deepOrangeA400, deepOrangeA700


## Brown Hue

@docs brown50, brown100, brown200, brown300, brown400, brown500, brown600, brown700, brown800, brown900


## Grey Hue

@docs grey50, grey100, grey200, grey300, grey400, grey500, grey600, grey700, grey800, grey900


## Blue Grey Hue

@docs blueGrey50, blueGrey100, blueGrey200, blueGrey300, blueGrey400, blueGrey500, blueGrey600, blueGrey700, blueGrey800, blueGrey900

-}

import Element


{-| Record to hold RGB data as Int fields
-}
type alias Rgba255 =
    Element.Color


{-| Rgb255 constructor helper
-}
rgb255 : Int -> Int -> Int -> Element.Color
rgb255 =
    Element.rgb255



-- RED


{-| Red 50
-}
red50 : Rgba255
red50 =
    rgb255 255 235 238


{-| Red 100
-}
red100 : Rgba255
red100 =
    rgb255 255 205 210


{-| Red 200
-}
red200 : Rgba255
red200 =
    rgb255 239 154 154


{-| Red 300
-}
red300 : Rgba255
red300 =
    rgb255 229 115 115


{-| Red 400
-}
red400 : Rgba255
red400 =
    rgb255 239 83 80


{-| Red 500
-}
red500 : Rgba255
red500 =
    rgb255 244 67 54


{-| Red 600
-}
red600 : Rgba255
red600 =
    rgb255 229 57 53


{-| Red 700
-}
red700 : Rgba255
red700 =
    rgb255 211 47 47


{-| Red 800
-}
red800 : Rgba255
red800 =
    rgb255 198 40 40


{-| Red 900
-}
red900 : Rgba255
red900 =
    rgb255 183 28 28


{-| Red A100
-}
redA100 : Rgba255
redA100 =
    rgb255 255 138 128


{-| Red A200
-}
redA200 : Rgba255
redA200 =
    rgb255 255 82 82


{-| Red A400
-}
redA400 : Rgba255
redA400 =
    rgb255 255 23 68


{-| Red A700
-}
redA700 : Rgba255
redA700 =
    rgb255 213 0 0



-- PINK


{-| Pink 50
-}
pink50 : Rgba255
pink50 =
    rgb255 252 228 236


{-| Pink 100
-}
pink100 : Rgba255
pink100 =
    rgb255 248 187 208


{-| Pink 200
-}
pink200 : Rgba255
pink200 =
    rgb255 244 143 177


{-| Pink 300
-}
pink300 : Rgba255
pink300 =
    rgb255 240 98 146


{-| Pink 400
-}
pink400 : Rgba255
pink400 =
    rgb255 236 64 122


{-| Pink 500
-}
pink500 : Rgba255
pink500 =
    rgb255 233 30 99


{-| Pink 600
-}
pink600 : Rgba255
pink600 =
    rgb255 216 27 96


{-| Pink 700
-}
pink700 : Rgba255
pink700 =
    rgb255 194 24 91


{-| Pink 800
-}
pink800 : Rgba255
pink800 =
    rgb255 173 20 87


{-| Pink 900
-}
pink900 : Rgba255
pink900 =
    rgb255 136 14 79


{-| Pink A100
-}
pinkA100 : Rgba255
pinkA100 =
    rgb255 255 128 171


{-| Pink A200
-}
pinkA200 : Rgba255
pinkA200 =
    rgb255 255 64 129


{-| Pink A400
-}
pinkA400 : Rgba255
pinkA400 =
    rgb255 245 0 87


{-| Pink A700
-}
pinkA700 : Rgba255
pinkA700 =
    rgb255 197 17 98



-- PURPLE


{-| Purple 50
-}
purple50 : Rgba255
purple50 =
    rgb255 243 229 245


{-| Purple 100
-}
purple100 : Rgba255
purple100 =
    rgb255 225 190 231


{-| Purple 200
-}
purple200 : Rgba255
purple200 =
    rgb255 206 147 216


{-| Purple 300
-}
purple300 : Rgba255
purple300 =
    rgb255 186 104 200


{-| Purple 400
-}
purple400 : Rgba255
purple400 =
    rgb255 171 71 188


{-| Purple 500
-}
purple500 : Rgba255
purple500 =
    rgb255 156 39 176


{-| Purple 600
-}
purple600 : Rgba255
purple600 =
    rgb255 142 36 170


{-| Purple 700
-}
purple700 : Rgba255
purple700 =
    rgb255 123 31 162


{-| Purple 800
-}
purple800 : Rgba255
purple800 =
    rgb255 106 27 154


{-| Purple 900
-}
purple900 : Rgba255
purple900 =
    rgb255 74 20 140


{-| Purple A100
-}
purpleA100 : Rgba255
purpleA100 =
    rgb255 234 128 252


{-| Purple A200
-}
purpleA200 : Rgba255
purpleA200 =
    rgb255 224 64 251


{-| Purple A400
-}
purpleA400 : Rgba255
purpleA400 =
    rgb255 213 0 249


{-| Purple A700
-}
purpleA700 : Rgba255
purpleA700 =
    rgb255 170 0 255



-- DEEP PURPLE


{-| Deep Purple 50
-}
deepPurple50 : Rgba255
deepPurple50 =
    rgb255 237 231 246


{-| Deep Purple 100
-}
deepPurple100 : Rgba255
deepPurple100 =
    rgb255 209 196 233


{-| Deep Purple 200
-}
deepPurple200 : Rgba255
deepPurple200 =
    rgb255 179 157 219


{-| Deep Purple 300
-}
deepPurple300 : Rgba255
deepPurple300 =
    rgb255 149 117 205


{-| Deep Purple 400
-}
deepPurple400 : Rgba255
deepPurple400 =
    rgb255 126 87 194


{-| Deep Purple 500
-}
deepPurple500 : Rgba255
deepPurple500 =
    rgb255 103 58 183


{-| Deep Purple 600
-}
deepPurple600 : Rgba255
deepPurple600 =
    rgb255 94 53 177


{-| Deep Purple 700
-}
deepPurple700 : Rgba255
deepPurple700 =
    rgb255 81 45 168


{-| Deep Purple 800
-}
deepPurple800 : Rgba255
deepPurple800 =
    rgb255 69 39 160


{-| Deep Purple 900
-}
deepPurple900 : Rgba255
deepPurple900 =
    rgb255 49 27 146


{-| Deep Purple A100
-}
deepPurpleA100 : Rgba255
deepPurpleA100 =
    rgb255 179 136 255


{-| Deep Purple A200
-}
deepPurpleA200 : Rgba255
deepPurpleA200 =
    rgb255 124 77 255


{-| Deep Purple A400
-}
deepPurpleA400 : Rgba255
deepPurpleA400 =
    rgb255 101 31 255


{-| Deep Purple A700
-}
deepPurpleA700 : Rgba255
deepPurpleA700 =
    rgb255 98 0 234



-- INDIGO


{-| Indigo 50
-}
indigo50 : Rgba255
indigo50 =
    rgb255 232 234 246


{-| Indigo 100
-}
indigo100 : Rgba255
indigo100 =
    rgb255 197 202 233


{-| Indigo 200
-}
indigo200 : Rgba255
indigo200 =
    rgb255 159 168 218


{-| Indigo 300
-}
indigo300 : Rgba255
indigo300 =
    rgb255 121 134 203


{-| Indigo 400
-}
indigo400 : Rgba255
indigo400 =
    rgb255 92 107 192


{-| Indigo 500
-}
indigo500 : Rgba255
indigo500 =
    rgb255 63 81 181


{-| Indigo 600
-}
indigo600 : Rgba255
indigo600 =
    rgb255 57 73 171


{-| Indigo 700
-}
indigo700 : Rgba255
indigo700 =
    rgb255 48 63 159


{-| Indigo 800
-}
indigo800 : Rgba255
indigo800 =
    rgb255 40 53 147


{-| Indigo 900
-}
indigo900 : Rgba255
indigo900 =
    rgb255 26 35 126


{-| Indigo A100
-}
indigoA100 : Rgba255
indigoA100 =
    rgb255 140 158 255


{-| Indigo A200
-}
indigoA200 : Rgba255
indigoA200 =
    rgb255 83 109 254


{-| Indigo A400
-}
indigoA400 : Rgba255
indigoA400 =
    rgb255 61 90 254


{-| Indigo A700
-}
indigoA700 : Rgba255
indigoA700 =
    rgb255 48 79 254



-- BLUE


{-| Blue 50
-}
blue50 : Rgba255
blue50 =
    rgb255 227 242 253


{-| Blue 100
-}
blue100 : Rgba255
blue100 =
    rgb255 187 222 251


{-| Blue 200
-}
blue200 : Rgba255
blue200 =
    rgb255 144 202 249


{-| Blue 300
-}
blue300 : Rgba255
blue300 =
    rgb255 100 181 246


{-| Blue 400
-}
blue400 : Rgba255
blue400 =
    rgb255 66 165 245


{-| Blue 500
-}
blue500 : Rgba255
blue500 =
    rgb255 33 150 243


{-| Blue 600
-}
blue600 : Rgba255
blue600 =
    rgb255 30 136 229


{-| Blue 700
-}
blue700 : Rgba255
blue700 =
    rgb255 25 118 210


{-| Blue 800
-}
blue800 : Rgba255
blue800 =
    rgb255 21 101 192


{-| Blue 900
-}
blue900 : Rgba255
blue900 =
    rgb255 13 71 161


{-| Blue A100
-}
blueA100 : Rgba255
blueA100 =
    rgb255 130 177 255


{-| Blue A200
-}
blueA200 : Rgba255
blueA200 =
    rgb255 68 138 255


{-| Blue A400
-}
blueA400 : Rgba255
blueA400 =
    rgb255 41 121 255


{-| Blue A700
-}
blueA700 : Rgba255
blueA700 =
    rgb255 41 98 255



-- LIGHT BLUE


{-| Light Blue 50
-}
lightBlue50 : Rgba255
lightBlue50 =
    rgb255 225 245 254


{-| Light Blue 100
-}
lightBlue100 : Rgba255
lightBlue100 =
    rgb255 179 229 252


{-| Light Blue 200
-}
lightBlue200 : Rgba255
lightBlue200 =
    rgb255 129 212 250


{-| Light Blue 300
-}
lightBlue300 : Rgba255
lightBlue300 =
    rgb255 79 195 247


{-| Light Blue 400
-}
lightBlue400 : Rgba255
lightBlue400 =
    rgb255 41 182 246


{-| Light Blue 500
-}
lightBlue500 : Rgba255
lightBlue500 =
    rgb255 3 169 244


{-| Light Blue 600
-}
lightBlue600 : Rgba255
lightBlue600 =
    rgb255 3 155 229


{-| Light Blue 700
-}
lightBlue700 : Rgba255
lightBlue700 =
    rgb255 2 136 209


{-| Light Blue 800
-}
lightBlue800 : Rgba255
lightBlue800 =
    rgb255 2 119 189


{-| Light Blue 900
-}
lightBlue900 : Rgba255
lightBlue900 =
    rgb255 1 87 155


{-| Light Blue A100
-}
lightBlueA100 : Rgba255
lightBlueA100 =
    rgb255 128 216 255


{-| Light Blue A200
-}
lightBlueA200 : Rgba255
lightBlueA200 =
    rgb255 64 196 255


{-| Light Blue A400
-}
lightBlueA400 : Rgba255
lightBlueA400 =
    rgb255 0 176 255


{-| Light Blue A700
-}
lightBlueA700 : Rgba255
lightBlueA700 =
    rgb255 0 145 234



-- CYAN


{-| Cyan 50
-}
cyan50 : Rgba255
cyan50 =
    rgb255 224 247 250


{-| Cyan 100
-}
cyan100 : Rgba255
cyan100 =
    rgb255 178 235 242


{-| Cyan 200
-}
cyan200 : Rgba255
cyan200 =
    rgb255 128 222 234


{-| Cyan 300
-}
cyan300 : Rgba255
cyan300 =
    rgb255 77 208 225


{-| Cyan 400
-}
cyan400 : Rgba255
cyan400 =
    rgb255 38 198 218


{-| Cyan 500
-}
cyan500 : Rgba255
cyan500 =
    rgb255 0 188 212


{-| Cyan 600
-}
cyan600 : Rgba255
cyan600 =
    rgb255 0 172 193


{-| Cyan 700
-}
cyan700 : Rgba255
cyan700 =
    rgb255 0 151 167


{-| Cyan 800
-}
cyan800 : Rgba255
cyan800 =
    rgb255 0 131 143


{-| Cyan 900
-}
cyan900 : Rgba255
cyan900 =
    rgb255 0 96 100


{-| Cyan A100
-}
cyanA100 : Rgba255
cyanA100 =
    rgb255 132 255 255


{-| Cyan A200
-}
cyanA200 : Rgba255
cyanA200 =
    rgb255 24 255 255


{-| Cyan A400
-}
cyanA400 : Rgba255
cyanA400 =
    rgb255 0 229 255


{-| Cyan A700
-}
cyanA700 : Rgba255
cyanA700 =
    rgb255 0 184 212



-- TEAL


{-| Teal 50
-}
teal50 : Rgba255
teal50 =
    rgb255 224 242 241


{-| Teal 100
-}
teal100 : Rgba255
teal100 =
    rgb255 178 223 219


{-| Teal 200
-}
teal200 : Rgba255
teal200 =
    rgb255 128 203 196


{-| Teal 300
-}
teal300 : Rgba255
teal300 =
    rgb255 77 182 172


{-| Teal 400
-}
teal400 : Rgba255
teal400 =
    rgb255 38 166 154


{-| Teal 500
-}
teal500 : Rgba255
teal500 =
    rgb255 0 150 136


{-| Teal 600
-}
teal600 : Rgba255
teal600 =
    rgb255 0 137 123


{-| Teal 700
-}
teal700 : Rgba255
teal700 =
    rgb255 0 121 107


{-| Teal 800
-}
teal800 : Rgba255
teal800 =
    rgb255 0 105 92


{-| Teal 900
-}
teal900 : Rgba255
teal900 =
    rgb255 0 77 64


{-| Teal A100
-}
tealA100 : Rgba255
tealA100 =
    rgb255 167 255 235


{-| Teal A200
-}
tealA200 : Rgba255
tealA200 =
    rgb255 100 255 218


{-| Teal A400
-}
tealA400 : Rgba255
tealA400 =
    rgb255 29 233 182


{-| Teal A700
-}
tealA700 : Rgba255
tealA700 =
    rgb255 0 191 165



-- GREEN


{-| Green 50
-}
green50 : Rgba255
green50 =
    rgb255 232 245 233


{-| Green 100
-}
green100 : Rgba255
green100 =
    rgb255 200 230 201


{-| Green 200
-}
green200 : Rgba255
green200 =
    rgb255 165 214 167


{-| Green 300
-}
green300 : Rgba255
green300 =
    rgb255 129 199 132


{-| Green 400
-}
green400 : Rgba255
green400 =
    rgb255 102 187 106


{-| Green 500
-}
green500 : Rgba255
green500 =
    rgb255 76 175 80


{-| Green 600
-}
green600 : Rgba255
green600 =
    rgb255 67 160 71


{-| Green 700
-}
green700 : Rgba255
green700 =
    rgb255 56 142 60


{-| Green 800
-}
green800 : Rgba255
green800 =
    rgb255 46 125 50


{-| Green 900
-}
green900 : Rgba255
green900 =
    rgb255 27 94 32


{-| Green A100
-}
greenA100 : Rgba255
greenA100 =
    rgb255 185 246 202


{-| Green A200
-}
greenA200 : Rgba255
greenA200 =
    rgb255 105 240 174


{-| Green A400
-}
greenA400 : Rgba255
greenA400 =
    rgb255 0 230 118


{-| Green A700
-}
greenA700 : Rgba255
greenA700 =
    rgb255 0 200 83



-- LIGHT GREEN


{-| Light Green 50
-}
lightGreen50 : Rgba255
lightGreen50 =
    rgb255 241 248 233


{-| Light Green 100
-}
lightGreen100 : Rgba255
lightGreen100 =
    rgb255 220 237 200


{-| Light Green 200
-}
lightGreen200 : Rgba255
lightGreen200 =
    rgb255 197 225 165


{-| Light Green 300
-}
lightGreen300 : Rgba255
lightGreen300 =
    rgb255 174 213 129


{-| Light Green 400
-}
lightGreen400 : Rgba255
lightGreen400 =
    rgb255 156 204 101


{-| Light Green 500
-}
lightGreen500 : Rgba255
lightGreen500 =
    rgb255 139 195 74


{-| Light Green 600
-}
lightGreen600 : Rgba255
lightGreen600 =
    rgb255 124 179 66


{-| Light Green 700
-}
lightGreen700 : Rgba255
lightGreen700 =
    rgb255 104 159 56


{-| Light Green 800
-}
lightGreen800 : Rgba255
lightGreen800 =
    rgb255 85 139 47


{-| Light Green 900
-}
lightGreen900 : Rgba255
lightGreen900 =
    rgb255 51 105 30


{-| Light Green A100
-}
lightGreenA100 : Rgba255
lightGreenA100 =
    rgb255 204 255 144


{-| Light Green A200
-}
lightGreenA200 : Rgba255
lightGreenA200 =
    rgb255 178 255 89


{-| Light Green A400
-}
lightGreenA400 : Rgba255
lightGreenA400 =
    rgb255 118 255 3


{-| Light Green A700
-}
lightGreenA700 : Rgba255
lightGreenA700 =
    rgb255 100 221 23



-- LIME


{-| Lime 50
-}
lime50 : Rgba255
lime50 =
    rgb255 249 251 231


{-| Lime 100
-}
lime100 : Rgba255
lime100 =
    rgb255 240 244 195


{-| Lime 200
-}
lime200 : Rgba255
lime200 =
    rgb255 230 238 156


{-| Lime 300
-}
lime300 : Rgba255
lime300 =
    rgb255 220 231 117


{-| Lime 400
-}
lime400 : Rgba255
lime400 =
    rgb255 212 225 87


{-| Lime 500
-}
lime500 : Rgba255
lime500 =
    rgb255 205 220 57


{-| Lime 600
-}
lime600 : Rgba255
lime600 =
    rgb255 192 202 51


{-| Lime 700
-}
lime700 : Rgba255
lime700 =
    rgb255 175 180 43


{-| Lime 800
-}
lime800 : Rgba255
lime800 =
    rgb255 158 157 36


{-| Lime 900
-}
lime900 : Rgba255
lime900 =
    rgb255 130 119 23


{-| Lime A100
-}
limeA100 : Rgba255
limeA100 =
    rgb255 244 255 129


{-| Lime A200
-}
limeA200 : Rgba255
limeA200 =
    rgb255 238 255 65


{-| Lime A400
-}
limeA400 : Rgba255
limeA400 =
    rgb255 198 255 0


{-| Lime A700
-}
limeA700 : Rgba255
limeA700 =
    rgb255 174 234 0



-- YELLOW


{-| Yellow 50
-}
yellow50 : Rgba255
yellow50 =
    rgb255 255 253 231


{-| Yellow 100
-}
yellow100 : Rgba255
yellow100 =
    rgb255 255 249 196


{-| Yellow 200
-}
yellow200 : Rgba255
yellow200 =
    rgb255 255 245 157


{-| Yellow 300
-}
yellow300 : Rgba255
yellow300 =
    rgb255 255 241 118


{-| Yellow 400
-}
yellow400 : Rgba255
yellow400 =
    rgb255 255 238 88


{-| Yellow 500
-}
yellow500 : Rgba255
yellow500 =
    rgb255 255 235 59


{-| Yellow 600
-}
yellow600 : Rgba255
yellow600 =
    rgb255 253 216 53


{-| Yellow 700
-}
yellow700 : Rgba255
yellow700 =
    rgb255 251 192 45


{-| Yellow 800
-}
yellow800 : Rgba255
yellow800 =
    rgb255 249 168 37


{-| Yellow 900
-}
yellow900 : Rgba255
yellow900 =
    rgb255 245 127 23


{-| Yellow A100
-}
yellowA100 : Rgba255
yellowA100 =
    rgb255 255 255 141


{-| Yellow A200
-}
yellowA200 : Rgba255
yellowA200 =
    rgb255 255 255 0


{-| Yellow A400
-}
yellowA400 : Rgba255
yellowA400 =
    rgb255 255 234 0


{-| Yellow A700
-}
yellowA700 : Rgba255
yellowA700 =
    rgb255 255 214 0



-- AMBER


{-| Amber 50
-}
amber50 : Rgba255
amber50 =
    rgb255 255 248 225


{-| Amber 100
-}
amber100 : Rgba255
amber100 =
    rgb255 255 236 179


{-| Amber 200
-}
amber200 : Rgba255
amber200 =
    rgb255 255 224 130


{-| Amber 300
-}
amber300 : Rgba255
amber300 =
    rgb255 255 213 79


{-| Amber 400
-}
amber400 : Rgba255
amber400 =
    rgb255 255 202 40


{-| Amber 500
-}
amber500 : Rgba255
amber500 =
    rgb255 255 193 7


{-| Amber 600
-}
amber600 : Rgba255
amber600 =
    rgb255 255 179 0


{-| Amber 700
-}
amber700 : Rgba255
amber700 =
    rgb255 255 160 0


{-| Amber 800
-}
amber800 : Rgba255
amber800 =
    rgb255 255 143 0


{-| Amber 900
-}
amber900 : Rgba255
amber900 =
    rgb255 255 111 0


{-| Amber A100
-}
amberA100 : Rgba255
amberA100 =
    rgb255 255 229 127


{-| Amber A200
-}
amberA200 : Rgba255
amberA200 =
    rgb255 255 215 64


{-| Amber A400
-}
amberA400 : Rgba255
amberA400 =
    rgb255 255 196 0


{-| Amber A700
-}
amberA700 : Rgba255
amberA700 =
    rgb255 255 171 0



-- ORANGE


{-| Orange 50
-}
orange50 : Rgba255
orange50 =
    rgb255 255 243 224


{-| Orange 100
-}
orange100 : Rgba255
orange100 =
    rgb255 255 224 178


{-| Orange 200
-}
orange200 : Rgba255
orange200 =
    rgb255 255 204 128


{-| Orange 300
-}
orange300 : Rgba255
orange300 =
    rgb255 255 183 77


{-| Orange 400
-}
orange400 : Rgba255
orange400 =
    rgb255 255 167 38


{-| Orange 500
-}
orange500 : Rgba255
orange500 =
    rgb255 255 152 0


{-| Orange 600
-}
orange600 : Rgba255
orange600 =
    rgb255 251 140 0


{-| Orange 700
-}
orange700 : Rgba255
orange700 =
    rgb255 245 124 0


{-| Orange 800
-}
orange800 : Rgba255
orange800 =
    rgb255 239 108 0


{-| Orange 900
-}
orange900 : Rgba255
orange900 =
    rgb255 230 81 0


{-| Orange A100
-}
orangeA100 : Rgba255
orangeA100 =
    rgb255 255 209 128


{-| Orange A200
-}
orangeA200 : Rgba255
orangeA200 =
    rgb255 255 171 64


{-| Orange A400
-}
orangeA400 : Rgba255
orangeA400 =
    rgb255 255 145 0


{-| Orange A700
-}
orangeA700 : Rgba255
orangeA700 =
    rgb255 255 109 0



-- DEEP ORANGE


{-| Deep Orange 50
-}
deepOrange50 : Rgba255
deepOrange50 =
    rgb255 251 233 231


{-| Deep Orange 100
-}
deepOrange100 : Rgba255
deepOrange100 =
    rgb255 255 204 188


{-| Deep Orange 200
-}
deepOrange200 : Rgba255
deepOrange200 =
    rgb255 255 171 145


{-| Deep Orange 300
-}
deepOrange300 : Rgba255
deepOrange300 =
    rgb255 255 138 101


{-| Deep Orange 400
-}
deepOrange400 : Rgba255
deepOrange400 =
    rgb255 255 112 67


{-| Deep Orange 500
-}
deepOrange500 : Rgba255
deepOrange500 =
    rgb255 255 87 34


{-| Deep Orange 600
-}
deepOrange600 : Rgba255
deepOrange600 =
    rgb255 244 81 30


{-| Deep Orange 700
-}
deepOrange700 : Rgba255
deepOrange700 =
    rgb255 230 74 25


{-| Deep Orange 800
-}
deepOrange800 : Rgba255
deepOrange800 =
    rgb255 216 67 21


{-| Deep Orange 900
-}
deepOrange900 : Rgba255
deepOrange900 =
    rgb255 191 54 12


{-| Deep Orange A100
-}
deepOrangeA100 : Rgba255
deepOrangeA100 =
    rgb255 255 158 128


{-| Deep Orange A200
-}
deepOrangeA200 : Rgba255
deepOrangeA200 =
    rgb255 255 110 64


{-| Deep Orange A400
-}
deepOrangeA400 : Rgba255
deepOrangeA400 =
    rgb255 255 61 0


{-| Deep Orange A700
-}
deepOrangeA700 : Rgba255
deepOrangeA700 =
    rgb255 221 44 0



-- BROWN


{-| Brown 50
-}
brown50 : Rgba255
brown50 =
    rgb255 239 235 233


{-| Brown 100
-}
brown100 : Rgba255
brown100 =
    rgb255 215 204 200


{-| Brown 200
-}
brown200 : Rgba255
brown200 =
    rgb255 188 170 164


{-| Brown 300
-}
brown300 : Rgba255
brown300 =
    rgb255 161 136 127


{-| Brown 400
-}
brown400 : Rgba255
brown400 =
    rgb255 141 110 99


{-| Brown 500
-}
brown500 : Rgba255
brown500 =
    rgb255 121 85 72


{-| Brown 600
-}
brown600 : Rgba255
brown600 =
    rgb255 109 76 65


{-| Brown 700
-}
brown700 : Rgba255
brown700 =
    rgb255 93 64 55


{-| Brown 800
-}
brown800 : Rgba255
brown800 =
    rgb255 78 52 46


{-| Brown 900
-}
brown900 : Rgba255
brown900 =
    rgb255 62 39 35



-- GREY


{-| Grey 50
-}
grey50 : Rgba255
grey50 =
    rgb255 250 250 250


{-| Grey 100
-}
grey100 : Rgba255
grey100 =
    rgb255 245 245 245


{-| Grey 200
-}
grey200 : Rgba255
grey200 =
    rgb255 238 238 238


{-| Grey 300
-}
grey300 : Rgba255
grey300 =
    rgb255 224 224 224


{-| Grey 400
-}
grey400 : Rgba255
grey400 =
    rgb255 189 189 189


{-| Grey 500
-}
grey500 : Rgba255
grey500 =
    rgb255 158 158 158


{-| Grey 600
-}
grey600 : Rgba255
grey600 =
    rgb255 117 117 117


{-| Grey 700
-}
grey700 : Rgba255
grey700 =
    rgb255 97 97 97


{-| Grey 800
-}
grey800 : Rgba255
grey800 =
    rgb255 66 66 66


{-| Grey 900
-}
grey900 : Rgba255
grey900 =
    rgb255 33 33 33



-- BLUE GREY


{-| Blue Grey 50
-}
blueGrey50 : Rgba255
blueGrey50 =
    rgb255 236 239 241


{-| Blue Grey 100
-}
blueGrey100 : Rgba255
blueGrey100 =
    rgb255 207 216 220


{-| Blue Grey 200
-}
blueGrey200 : Rgba255
blueGrey200 =
    rgb255 176 190 197


{-| Blue Grey 300
-}
blueGrey300 : Rgba255
blueGrey300 =
    rgb255 144 164 174


{-| Blue Grey 400
-}
blueGrey400 : Rgba255
blueGrey400 =
    rgb255 120 144 156


{-| Blue Grey 500
-}
blueGrey500 : Rgba255
blueGrey500 =
    rgb255 96 125 139


{-| Blue Grey 600
-}
blueGrey600 : Rgba255
blueGrey600 =
    rgb255 84 110 122


{-| Blue Grey 700
-}
blueGrey700 : Rgba255
blueGrey700 =
    rgb255 69 90 100


{-| Blue Grey 800
-}
blueGrey800 : Rgba255
blueGrey800 =
    rgb255 55 71 79


{-| Blue Grey 900
-}
blueGrey900 : Rgba255
blueGrey900 =
    rgb255 38 50 56
