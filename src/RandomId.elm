module RandomId exposing (gen, stringIdGenerator)

import Random


idChars : List Char
idChars =
    let
        alphabets =
            { lowercase = "abcdefghijklmnopqrstuvwxyz"
            , uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            }

        numbers =
            "0123456789"
    in
    alphabets.lowercase ++ alphabets.uppercase ++ numbers ++ "_" |> String.toList


idCharGenerator : Random.Generator Char
idCharGenerator =
    Random.uniform '~' idChars


stringIdGenerator : Random.Generator String
stringIdGenerator =
    Random.list 21 idCharGenerator |> Random.map String.fromList


gen : (String -> msg) -> Cmd msg
gen toMsg =
    Random.generate toMsg stringIdGenerator
