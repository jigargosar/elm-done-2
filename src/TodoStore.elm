module TodoStore exposing (Model)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Port
import Random
import RandomId
import TimeX exposing (Millis)
import Todo


type alias ModelRecord =
    { lookup : Dict String Todo.Model
    }


type Model
    = Model ModelRecord


type alias Encoder =
    Model -> Value


encoder : Encoder
encoder (Model model) =
    E.object
        [ ( "lookup", E.dict identity Todo.encoder model.lookup )
        ]


decoder : Decoder Model
decoder =
    D.map ModelRecord
        (D.field "lookup" <| D.dict Todo.decoder)
        |> D.map Model


type alias TodoBuilder =
    { id : Maybe String
    , now : Maybe Millis
    , title : String
    , contextId : String
    }


type alias HasMaybeIdNow x =
    { x
        | id : Maybe String
        , now : Maybe Millis
    }


setJustId : HasMaybeIdNow x -> String -> HasMaybeIdNow x
setJustId model id =
    { model | id = Just id }


setJustNow : HasMaybeIdNow x -> Millis -> HasMaybeIdNow x
setJustNow model now =
    { model | now = Just now }


type Msg
    = New TodoBuilder


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    updateF message << pure


updateF : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateF message =
    case message of
        New builder ->
            case ( builder.id, builder.now ) of
                ( Nothing, _ ) ->
                    addCmd <| RandomId.gen (New << setJustId builder)

                ( _, Nothing ) ->
                    addCmd <| TimeX.now (New << setJustNow builder)

                ( Just id, Just now ) ->
                    let
                        todo : Todo.Model
                        todo =
                            Todo.init
                                { id = id
                                , createdAt = now
                                , modifiedAt = now
                                , title = builder.title
                                , body = ""
                                , done = False
                                , contextId = builder.contextId
                                }
                    in
                    andThen <| upsertAndCache todo


upsertAndCache : Todo.Model -> Model -> ( Model, Cmd Msg )
upsertAndCache todo (Model model) =
    pure (Model { model | lookup = Dict.insert (Todo.idString todo) todo model.lookup })
        |> effect (Port.cacheTodoStore << encoder)


pure m =
    ( m, Cmd.none )


effect f ( m, c ) =
    ( m, c ) |> addCmd (f m)


addCmd c2 ( m, c ) =
    ( m, Cmd.batch [ c, c2 ] )


mapModel =
    Tuple.mapFirst


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ] )
