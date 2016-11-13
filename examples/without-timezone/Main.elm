module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Time exposing (Time)
import Time.DateTime as DateTime exposing (DateTime)


type alias Flags =
    { now : Time }


type alias Model =
    { now : DateTime
    }


type Msg
    = Tick Time


main : Program Flags Model Msg
main =
    H.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    { now = DateTime.fromTimestamp now
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            { model | now = DateTime.fromTimestamp now } ! []


view : Model -> Html Msg
view { now } =
    H.div []
        [ DateTime.toISO8601 now
            |> H.text
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every Time.second Tick ]
