module BasicsX exposing
    ( Getter
    , Millis
    , Pred
    , PredList
    , allPass
    , applyMaybe
    , applyMaybeFn2
    , applyTo
    , applyTo2
    , defaultEmptyStringTo
    , eq0
    , eqs
    , flip
    , ifElse
    , isWhitespaceOrEmptyString
    , maybeBool
    , maybeWhen
    , notPred
    , propEq
    , replaceHead
    , safeModBy
    , swap
    , ter
    , unless
    , unpackMaybe
    , unpackResult
    , unwrapMaybe
    , when
    )


type alias Pred a =
    a -> Bool


type alias PredList a =
    List (Pred a)


allPass : PredList a -> Pred a
allPass plist a =
    List.all (applyTo a) plist


notPred : Pred a -> Pred a
notPred pred =
    pred >> not


type alias Getter big small =
    big -> small


propEq : Getter big small -> small -> Pred big
propEq getter small =
    getter >> eqs small


replaceHead newHead list =
    case list of
        head :: tail ->
            newHead :: tail

        [] ->
            []


type alias Millis =
    Int


ter b t f =
    if b then
        t

    else
        f


applyTo a fn =
    fn a


applyTo2 : a -> b -> (a -> b -> c) -> c
applyTo2 a b fn =
    fn a b


applyMaybeFn2 : a -> b -> Maybe (a -> b -> c) -> Maybe c
applyMaybeFn2 a b =
    Maybe.map (applyTo2 a b)


applyMaybe a =
    Maybe.map (applyTo a)


ifElse b t f v =
    ter (b v) (t v) (f v)


defaultEmptyStringTo : String -> String -> String
defaultEmptyStringTo =
    when String.isEmpty << always


when : (a -> Bool) -> (a -> a) -> a -> a
when b t =
    ifElse b t identity


unless b =
    when (b >> not)


eqs =
    (==)


eq0 =
    eqs 0


maybeBool bool value =
    if bool then
        Just value

    else
        Nothing


maybeWhen pred answerFn =
    ifElse pred (answerFn >> Just) (always Nothing)


unwrapMaybe : b -> (a -> b) -> Maybe a -> b
unwrapMaybe dv fn =
    Maybe.map fn >> Maybe.withDefault dv


unpackMaybe dvFn fn mb =
    case mb of
        Nothing ->
            dvFn

        Just val ->
            fn val


unpackResult errFn okFn result =
    case result of
        Ok answer ->
            okFn answer

        Err error ->
            errFn error


flip fn a b =
    fn b a


swap ( a, b ) =
    ( b, a )


isWhitespaceOrEmptyString =
    String.trim >> String.isEmpty


safeModBy total num =
    if total == 0 then
        0

    else
        modBy total num
