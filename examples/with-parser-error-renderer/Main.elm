module Main exposing (main)

{-| An Elm web client application to exercise
the `elm-time` library.


# Main entry

@docs main

-}

import Char
import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events
import Element.Input as Input
import Html exposing (Html)
import Keyboard exposing (KeyCode)
import Parser exposing (Error)
import String
import Style exposing (..)
import Style.Border as Border
import Style.Color
import Style.Font as Font
import Style.Transition as Transition
import Task
import Time.Date
import Time.DateTime as TD
    exposing
        ( DateTime
        , dateTime
        , fromTimestamp
        , fromTuple
        , year
        , month
        )
import Time.Internal exposing (DateTimeData)
import Time.ISO8601Error exposing (reflow, renderText)
import Time.Iso8601 exposing (toDateTime)
import Window


type Msg
    = ChangeText String
    | Resize Window.Size
    | KeyDown Keyboard.KeyCode


type Styles
    = None
    | InputContainer
    | Page
    | DateTimeGrid
    | DateTimeRow
    | Box
    | Error


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style InputContainer
            [ Style.Color.text black
            , Style.Color.background lightGray
            , Border.all 2
            ]
        , style Error
            [ Style.Color.text red
            , Style.Color.background white
            , Font.typeface [ Font.monospace ]
            ]
        , style DateTimeGrid
            [ Border.top 2
            , Border.bottom 2
            , Border.left 1
            , Border.right 1
            ]
        , style Box
            [ Border.left 1
            , Border.right 1
            , Style.Color.background white
            , Font.center
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
    , dateTime : Result Error DateTime
    , device : Device
    }


init : ( Model, Cmd Msg )
init =
    let
        initInput =
            "1991-02-28T12:25:12.0Z"

        initDateTime =
            TD.dateTime
                { year = 1991
                , month = 2
                , day = 28
                , hour = 12
                , minute = 25
                , second = 12
                , millisecond = 0
                }
    in
        ( { iso8601input = initInput
          , dateTime = Ok initDateTime
          , device = classifyDevice (Window.Size 0 0)
          }
        , Task.perform Resize Window.size
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText text ->
            ( { model
                | iso8601input = text
              }
            , Cmd.none
            )

        Resize size ->
            ( { model
                | device = classifyDevice size
              }
            , Cmd.none
            )

        KeyDown keyCode ->
            if enterKeyCode == keyCode then
                runParse model
            else
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        el None [ center, width (px 800) ] <|
            column Page
                [ spacing 20 ]
                [ Input.text InputContainer
                    [ padding 10 ]
                    { onChange = ChangeText
                    , value = model.iso8601input
                    , label =
                        Input.placeholder
                            { label = Input.labelLeft (el None [ verticalCenter ] (text "Change ISO8601 string, then hit Enter:"))
                            , text = "Test ISO8601 here"
                            }
                    , options = []
                    }
                , case model.dateTime of
                    Ok v ->
                        renderDateTimeSuccess v

                    Err err ->
                        renderDateTimeFail err
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Keyboard.downs KeyDown
        ]


runParse : Model -> ( Model, Cmd Msg )
runParse model =
    case toDateTime model.iso8601input of
        Ok v ->
            ( { model
                | dateTime = Ok v
              }
            , Cmd.none
            )

        Err msg ->
            ( { model
                | dateTime = Err msg
              }
            , Cmd.none
            )


renderDateTimeSuccess : DateTime -> Element Styles a b
renderDateTimeSuccess dt =
    let
        renderCell x y value =
            cell
                { start = (x, y)
                , width = 1
                , height = 1
                , content = el Box [] (text <| value)
                }
    in
        grid DateTimeGrid
            []
            { columns = [ px 100, px 100, px 100 ]
            , rows = [ px 20, px 20 ]
            , cells =
                [ renderCell 0 0 "Year"
                , renderCell 1 0 "Month"
                , renderCell 2 0 "Day"
                , renderCell 3 0 "Hour"
                , renderCell 4 0 "Minute"
                , renderCell 5 0 "Second"
                , renderCell 6 0 "Millisecond"
                , renderCell 0 1 (toString <| TD.year dt)
                , renderCell 1 1 (toString <| TD.month dt)
                , renderCell 2 1 (toString <| TD.day dt)
                , renderCell 3 1 (toString <| TD.hour dt)
                , renderCell 4 1 (toString <| TD.minute dt)
                , renderCell 5 1 (toString <| TD.second dt)
                , renderCell 6 1 (toString <| TD.millisecond dt)
                ]
            }



--    el Success [] (text <| toString dateTime)


renderDateTimeFail : Error -> Element Styles a b
renderDateTimeFail err =
    el Error [] (text <| renderText err)


render : Model -> String
render model =
    case model.dateTime of
        Ok dt ->
            toString dt

        Err err ->
            renderText err
                ++ "\n\n"
                ++ reflow (toString err)


enterKeyCode : KeyCode
enterKeyCode =
    13
