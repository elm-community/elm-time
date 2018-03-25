module Main exposing (main)

{-| An Elm web client application to exercise
the `elm-time` library.


# Main entry

@docs main

-}

import Color exposing (rgba)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Parser exposing (Error)
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
    = Run
    | Tick Time


type Styles
    = None
    | Container
    | Field
    | SubMenu
    | Error
    | InputError
    | Checkbox
    | CheckboxChecked
    | LabelBox
    | Button
    | CustomRadio



{-
   , red, orange, yellow, green, blue, purple, brown
   , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
   , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
   , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
   , lightGray, gray, darkGray
-}


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Container
            [ Color.text Color.black
            , Color.background Color.white
            ]
        , style Error
            [ Color.text Color.red
            ]
        , style CustomRadio
            [ Border.rounded 5
            ]
        ]


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
    , dateTime : Result Parser.Error DateTime
    }


init : ( Model, Cmd Msg )
init =
    let
        initInput =
            "1991-02-29T12:25:12.0Z"
    in
    ( { iso8601input = initInput
      , dateTime = fromISO8601 initInput
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            runParse model

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
    case model.dateTime of
        Ok dt ->
            Html.text <| toString dt

        Err err ->
            let
                msg =
                    renderText err
                        ++ "\n\n"
                        ++ reflow (toString err)
            in
            Html.pre [] [ Html.text <| msg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    every Time.second Tick


runParse : Model -> ( Model, Cmd Msg )
runParse model =
    ( { model
        | dateTime = fromISO8601 model.iso8601input
      }
    , Cmd.none
    )
