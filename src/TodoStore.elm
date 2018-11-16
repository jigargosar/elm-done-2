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


type IdNow
    = IdNow (Maybe String) (Maybe Millis)


type alias TodoBuilder =
    { idNow : IdNow
    , title : String
    , contextId : String
    }


setId maybeNow model id =
    { model | idNow = IdNow (Just id) maybeNow }


setNow maybeId model now =
    { model | idNow = IdNow maybeId (Just now) }


type Msg
    = New TodoBuilder


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        New builder ->
            case builder.idNow of
                IdNow Nothing maybeNow ->
                    ( model
                    , RandomId.gen (setId maybeNow builder >> New)
                    )

                IdNow maybeId Nothing ->
                    ( model, TimeX.now (setNow maybeId builder >> New) )

                IdNow (Just id) (Just now) ->
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
                    upsertAndCache todo model


pure m =
    ( m, Cmd.none )


upsertAndCache : Todo.Model -> Model -> ( Model, Cmd Msg )
upsertAndCache todo (Model model) =
    pure (Model { model | lookup = Dict.insert (Todo.idString todo) todo model.lookup })
        |> effect (Port.cacheTodoStore << encoder)


effect f ( m, c ) =
    ( m, Cmd.batch [ c, f m ] )


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ] )
