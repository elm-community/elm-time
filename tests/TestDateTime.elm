module TestDateTime exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (intRange)
import Test exposing (..)
import UTC.DateTime exposing (..)


toFromISO8601 : Test
toFromISO8601 =
    let
        parseEq input dateTime =
            case ( fromISO8601 input, dateTime ) of
                ( Err _, Nothing ) ->
                    Expect.pass

                ( Ok date1, Just date2 ) ->
                    if date1 == date2 then
                        Expect.pass
                    else
                        Expect.fail ("expected '" ++ toISO8601 date1 ++ "' to equal '" ++ toISO8601 date2 ++ "' from input '" ++ input ++ "'")

                ( Err message, _ ) ->
                    Expect.fail (message ++ " in input '" ++ input ++ "'")

                ( Ok _, Nothing ) ->
                    Expect.fail ("parsing '" ++ input ++ "' should have failed")

        parseFails input =
            case fromISO8601 input of
                Err _ ->
                    Expect.pass

                Ok _ ->
                    Expect.fail ("parsing '" ++ input ++ "' should have failed")
    in
        describe "UTC.DateTime.{to,from}ISO8601"
            [ test "fromISO8601 is the inverse of toISO8601" <|
                \() -> toISO8601 epoch |> flip parseEq (Just epoch)
            , test "formISO8601 fails to parse invalid strings" <|
                \() -> parseFails "foo"
            , test "formISO8601 fails to parse invalid UTC datetimes" <|
                \() -> parseFails "1993-02-29T00:00:00Z"
            , test "fromISO8601 can parse valid UTC datetime strings" <|
                \() -> parseEq "1992-05-29T12:25:12Z" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 12, minute = 25, second = 12 })
            , fuzz2 (intRange -23 23) (intRange 0 59) "fromISO8601 parses offsets correctly" <|
                \hour minute ->
                    let
                        sign =
                            if hour < 0 then
                                -1
                            else
                                1

                        signStr =
                            if hour < 0 then
                                "+"
                            else
                                "-"

                        padded n =
                            if abs n < 10 then
                                "0" ++ toString (abs n)
                            else
                                toString (abs n)

                        input =
                            "1992-05-29T12:25:12" ++ signStr ++ padded hour ++ ":" ++ padded minute

                        output =
                            dateTime { zero | year = 1992, month = 5, day = 29, hour = 12, minute = 25, second = 12 }
                                |> Maybe.map (addHours hour)
                                |> Maybe.map (addMinutes (sign * minute))
                    in
                        parseEq input output
            ]


all : Test
all =
    describe "UTC.DateTime"
        [ toFromISO8601 ]
