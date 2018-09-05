module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Time exposing (Posix)
import Time.DateTime as DateTime exposing (DateTime)
import Time.Iso8601


type alias Flags =
    { now : Int }


type alias Model =
    { now : DateTime
    }


type Msg
    = Tick Posix


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
    ( { now = now |> Time.millisToPosix |> DateTime.fromPosix
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            ( { model | now = DateTime.fromPosix now }
            , Cmd.none
            )


view : Model -> Html Msg
view { now } =
    H.div []
        [ Time.Iso8601.fromDateTime now
            |> H.text
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 1000 Tick ]
