module TodoStore exposing (Model)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
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
    , content : String
    , contextId : String
    }


type Msg
    = New TodoBuilder


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        New builder ->
            case builder.idNow of
                IdNow Nothing _ ->
                    ( model, Cmd.none )

                IdNow _ Nothing ->
                    ( model, Cmd.none )

                IdNow (Just id) (Just now) ->
                    ( model, Cmd.none )
