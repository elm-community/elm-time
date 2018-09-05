module Main exposing (main)

{-| An Elm web client application to exercise
the `elm-time` library.


# Main entry

@docs main

-}

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as JD
import Parser.Advanced exposing (DeadEnd)
import Style exposing (..)
import Style.Border as Border
import Style.Color
import Style.Font as Font
import Task
import Time.DateTime
    exposing
        ( DateTime
        , dateTime
        , day
        , hour
        , millisecond
        , minute
        , month
        , second
        , year
        )
import Time.Iso8601 exposing (Problem, toDateTime)
import Time.Iso8601ErrorMsg exposing (reflow, renderText)


type Msg
    = ChangeText String
    | Resize Int Int
    | EnterPressed
    | NoOp


type Styles
    = None
    | InputContainer
    | Page
    | DateTimeGrid
    | DateTimeRow
    | Box
    | Error


black =
    Style.rgb 0 0 0


red =
    Style.rgb 1 0 0


lightGray =
    Style.rgb 0.8 0.8 0.8


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style Page
            []
        , style InputContainer
            [ Style.Color.text black
            , Style.Color.background lightGray
            , Border.all 2
            ]
        , style Error
            [ Style.Color.text red
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
            , Font.center
            ]
        ]


{-| The "entry"
-}
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { iso8601input : String
    , dateTime : Result (List (DeadEnd String Problem)) DateTime
    , device : Device
    }


init : ( Model, Cmd Msg )
init =
    let
        initInput =
            "1991-02-28T12:25:12.0Z"

        initDateTime =
            dateTime
                { year = 1991
                , month = 2
                , day = 28
                , hour = 12
                , minute = 25
                , second = 12
                , millisecond = 0
                }

        getViewport =
            Task.perform (\vp -> Resize (round vp.viewport.width) (round vp.viewport.height)) Browser.Dom.getViewport
    in
    ( { iso8601input = initInput
      , dateTime = Ok initDateTime
      , device = classifyDevice { width = 0, height = 0 }
      }
    , getViewport
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

        Resize width height ->
            ( { model
                | device = classifyDevice { width = width, height = height }
              }
            , Cmd.none
            )

        EnterPressed ->
            runParse model

        NoOp ->
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
                        renderDateTimeFail model.iso8601input err
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        decoder : JD.Decoder Msg
        decoder =
            JD.field "key" JD.string
                |> JD.map
                    (\key ->
                        if key == "Enter" then
                            EnterPressed

                        else
                            NoOp
                    )
    in
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onKeyDown decoder
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
                { start = ( x, y )
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
            , renderCell 0 1 (String.fromInt <| year dt)
            , renderCell 1 1 (String.fromInt <| month dt)
            , renderCell 2 1 (String.fromInt <| day dt)
            , renderCell 3 1 (String.fromInt <| hour dt)
            , renderCell 4 1 (String.fromInt <| minute dt)
            , renderCell 5 1 (String.fromInt <| second dt)
            , renderCell 6 1 (String.fromInt <| millisecond dt)
            ]
        }



--    el Success [] (text <| Debug.toString dateTime)


renderDateTimeFail : String -> List (DeadEnd String Problem) -> Element Styles a b
renderDateTimeFail input errors =
    List.map (renderText input) errors
        |> String.join "\n\n---\n\n"
        |> text
        |> el Error []
