module Main exposing (main)

{-| An Elm web client application to exercise
the `elm-time` library.


# Main entry

@docs main

-}

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events
import Element.Input as Input
import Html exposing (Html)
import Parser exposing (Error)
import String
import Style exposing (..)
import Style.Border as Border
import Style.Color
import Style.Font as Font
import Style.Transition as Transition
import Task
import Time.DateTime
    exposing
        ( DateTime
        , fromTimestamp
        , fromTuple
        )
import Time.ISO8601Error exposing (reflow, renderText)
import Time.Iso8601 exposing (toDateTime)
import Window


type Msg
    = Run
    | ChangeText String
    | Resize Window.Size


type Styles
    = None
    | InputContainer
    | Page
    | Title
    | Field
    | SubMenu
    | Error
    | InputError
    | LabelBox
    | Button



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
        , style InputContainer
            [ Style.Color.text black
            , Style.Color.background lightGreen
            ]
        , style Error
            [ Style.Color.text red
            , Font.typeface [ Font.monospace ]
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
    , device : Device
    }


init : ( Model, Cmd Msg )
init =
    let
        initInput =
            "1991-02-29T12:25:12.0Z"
    in
        ( { iso8601input = initInput
          , dateTime = toDateTime initInput
          , device = classifyDevice (Window.Size 0 0)
          }
        , Task.perform Resize Window.size
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            runParse model

        ChangeText text ->
            let
                endChar =
                    String.right 1 text
            in
                (if (True || endChar == "\n") then
                    { model
                        | iso8601input = text
                    }
                 else
                    model
                )
                    |> runParse

        Resize size ->
            ( { model
                | device = classifyDevice size
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        el None [ center, width (px 800) ] <|
            column Page
                [ spacing 20 ]
                [ Input.text Field
                    [ padding 10 ]
                    { onChange = ChangeText
                    , value = model.iso8601input
                    , label =
                        Input.placeholder
                            { label = Input.labelLeft (el None [ verticalCenter ] (text "Enter ISO8601 String:"))
                            , text = "Test ISO8601 here"
                            }
                    , options = []
                    }
                , el Error [] (text <| output model)
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes Resize


output : Model -> String
output model =
    case model.dateTime of
        Ok v ->
            toString v

        Err error ->
            renderText error


runParse : Model -> ( Model, Cmd Msg )
runParse model =
    ( { model
        | dateTime = toDateTime model.iso8601input
      }
    , Cmd.none
    )


render : Model -> String
render model =
    case model.dateTime of
        Ok dt ->
            toString dt

        Err err ->
            renderText err
                ++ "\n\n"
                ++ reflow (toString err)
