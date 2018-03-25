module Main exposing (main)

{-| An Elm web client application to exercise
the `elm-time` library.


# Main entry

@docs main

-}

import Color
import Element exposing (..)
import Element.Attributes exposing(..)
import Html exposing (Html)
import Parser
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import Time exposing (Time, every)
import Time.DateTime
    exposing
        ( DateTime
        , fromISO8601
        , fromTimestamp
        , fromTuple
        , toISO8601
        )
import Time.ISO8601Error exposing (reflow, renderText)


type Msg
    = Tick Time


{-| The "entry"
-}
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { iso8601input : String
    , result : DateTime
    }


init : ( Model, Cmd Msg )
init =
    ( { iso8601input = "1991-02-28T12:25:12.0Z"
      , result = fromTuple ( 1991, 2, 28, 12, 15, 12, 0 )
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model
                | iso8601input =
                    time
                        |> fromTimestamp
                        |> toISO8601
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case fromISO8601 model.iso8601input of
        Ok v ->
            Html.text <| toString v

        Err e ->
            let
                msg =
                    renderText e
                        ++ "\n\n"
                        ++ reflow (toString e)
            in
            Html.pre [] [ Html.text <| msg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    every Time.second Tick
