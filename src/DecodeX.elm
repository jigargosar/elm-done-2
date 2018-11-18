module DecodeX exposing (andMap, required)

import BasicsX exposing (callOn)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    D.map2 callOn


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required name =
    andMap << D.field name
