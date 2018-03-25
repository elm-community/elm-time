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
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


type alias Model =
    { iso8601input : String
    , dateTime : Result Parser.Error DateTime
    }


init : ( Model, Cmd Msg )
init =
    let
        initInput =
            "1991-02-28T12:25:12.0Z"
    in
    ( { iso8601input = initInput
      , dateTime = fromISO8601 initInput
      }
    , Cmd.none
    )


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg (model, msg2) =
    case msg of
        Run ->
            runParse model


view : ( Model, Cmd Msg ) -> Html Msg
view (model, msg) =
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


runParse : Model -> ( Model, Cmd Msg )
runParse model =
    ( { model
        | dateTime = fromISO8601 model.iso8601input
      }
    , Cmd.none
    )
