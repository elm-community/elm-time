module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Time exposing (Time)
import Time.DateTime as DateTime exposing (DateTime)
import Time.TimeZone exposing (TimeZone)
import Time.TimeZones as TimeZones
import Time.ZonedDateTime as ZonedDateTime


type alias Flags =
    { now : Time }


type alias Model =
    { now : DateTime
    , zone : TimeZone
    }


type Msg
    = Tick Time
    | ChangeZone String


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
    , zone = TimeZones.utc ()
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            { model | now = DateTime.fromTimestamp now } ! []

        ChangeZone zoneString ->
            let
                zone =
                    case zoneString of
                        "Europe/Bucharest" ->
                            TimeZones.europe_bucharest ()

                        "US/Central" ->
                            TimeZones.us_central ()

                        _ ->
                            TimeZones.utc ()
            in
                { model | zone = zone } ! []


view : Model -> Html Msg
view { now, zone } =
    H.div []
        [ H.select [ E.on "change" (JD.map ChangeZone E.targetValue) ]
            [ H.option [ A.value "UTC" ] [ H.text "UTC" ]
            , H.option [ A.value "Europe/Bucharest" ] [ H.text "Europe/Bucharest" ]
            , H.option [ A.value "US/Central" ] [ H.text "US/Central" ]
            ]
        , H.br [] []
        , ZonedDateTime.fromDateTime zone now
            |> ZonedDateTime.toISO8601
            |> H.text
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every Time.second Tick ]
