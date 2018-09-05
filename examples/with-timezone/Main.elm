module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Time exposing (Posix)
import Time.DateTime as DT exposing (DateTime)
import Time.Iso8601
import Time.TimeZone exposing (TimeZone)
import Time.TimeZones as TimeZones
import Time.ZonedDateTime


type alias Flags =
    { now : Int }


type alias Model =
    { now : DateTime
    , zone : TimeZone
    }


type Msg
    = Tick Posix
    | ChangeZone String
    | NoOp


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    ( { now = now |> Time.millisToPosix |> DT.fromPosix
      , zone = TimeZones.utc
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            ( { model | now = DT.fromPosix now }
            , Cmd.none
            )

        ChangeZone zoneString ->
            let
                zone =
                    case zoneString of
                        "Europe/Bucharest" ->
                            TimeZones.europe_bucharest

                        "US/Central" ->
                            TimeZones.us_central

                        _ ->
                            TimeZones.utc
            in
            ( { model | zone = zone }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { now, zone } =
    H.div []
        [ H.select [ E.on "change" (JD.map ChangeZone E.targetValue) ]
            [ H.option [ A.value "UTC" ] [ H.text "UTC" ]
            , H.option [ A.value "Europe/Bucharest" ] [ H.text "Europe/Bucharest" ]
            , H.option [ A.value "US/Central" ] [ H.text "US/Central" ]
            ]
        , H.br [] []
        , Time.ZonedDateTime.fromDateTime zone now
            |> Time.Iso8601.fromZonedDateTime
            |> H.text
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 1000 Tick ]
