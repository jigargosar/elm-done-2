module DecodeX exposing (Encoder, start)

import BasicsX exposing (callOn)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)



--andMap : Decoder a -> Decoder (a -> b) -> Decoder b
--andMap =
--    D.map2 callOn
--
--
--required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
--required name =
--    andMap << D.field name


type alias Encoder a =
    a -> Value


start =
    D.succeed
