module TestDateTime exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (intRange)
import Test exposing (..)
import Time.DateTime exposing (..)


dateTimes : Test
dateTimes =
    describe "Time.DateTime.{dateTime,epoch}"
        [ test "epoch is the epoch" <|
            \() -> Expect.equal (dateTime { zero | year = 1970 }) (Just epoch)
        , test "can construt valid dates" <|
            \() ->
                let
                    (( year, month, day, hour, minute, second, millisecond ) as dateTimeTuple1) =
                        ( 1992, 5, 29, 23, 29, 50, 920 )

                    dateTimeTuple2 =
                        dateTime { year = year, month = month, day = day, hour = hour, minute = minute, second = second, millisecond = millisecond }
                            |> Maybe.map toTuple
                in
                    Expect.equal (Just dateTimeTuple1) dateTimeTuple2
        , test "fails to construct invalid dates" <|
            \() -> Expect.equal Nothing (dateTime { zero | year = 1993, month = 2, day = 29 })
        ]


setters : Test
setters =
    describe "Time.DateTime.set{Year,Month,Day,Hour,Minute,Second,Millisecond}"
        [ test "can set year" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setYear 1992)
                    |> Maybe.map year
                    |> Expect.equal (Just 1992)
        , test "can set month" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setMonth 5)
                    |> Maybe.map month
                    |> Expect.equal (Just 5)
        , test "can set month given the current year and day" <|
            \() ->
                dateTime { zero | year = 1992, month = 1, day = 29 }
                    |> flip Maybe.andThen (setMonth 2)
                    |> Maybe.map month
                    |> Expect.equal (Just 2)
        , test "fails to set invalid months" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setMonth 31)
                    |> Expect.equal Nothing
        , test "fails to set invalid months given the current year and day" <|
            \() ->
                dateTime { zero | year = 1993, month = 1, day = 31 }
                    |> flip Maybe.andThen (setMonth 2)
                    |> Expect.equal Nothing
        , test "can set day" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setDay 15)
                    |> Maybe.map day
                    |> Expect.equal (Just 15)
        , test "can set day given the current year and month" <|
            \() ->
                dateTime { zero | year = 1992, month = 2 }
                    |> flip Maybe.andThen (setDay 29)
                    |> Maybe.map day
                    |> Expect.equal (Just 29)
        , test "fails to set invalid days given the current year and month" <|
            \() ->
                dateTime { zero | year = 1991, month = 2 }
                    |> flip Maybe.andThen (setDay 29)
                    |> Expect.equal Nothing
        , test "fails to set invalid days " <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setDay 105)
                    |> Expect.equal Nothing
        , test "can set hour" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setHour 23)
                    |> Maybe.map hour
                    |> Expect.equal (Just 23)
        , test "fails to set invalid hours" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setHour 49)
                    |> Expect.equal Nothing
        , test "can set minute" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setMinute 23)
                    |> Maybe.map minute
                    |> Expect.equal (Just 23)
        , test "fails to set invalid minutes" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setMinute 69)
                    |> Expect.equal Nothing
        , test "can set second" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setSecond 23)
                    |> Maybe.map second
                    |> Expect.equal (Just 23)
        , test "fails to set invalid seconds" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setSecond 69)
                    |> Expect.equal Nothing
        , test "can set millisecond" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setMillisecond 230)
                    |> Maybe.map millisecond
                    |> Expect.equal (Just 230)
        , test "fails to set invalid milliseconds" <|
            \() ->
                dateTime zero
                    |> flip Maybe.andThen (setMillisecond 6000)
                    |> Expect.equal Nothing
        ]


addition : Test
addition =
    describe "Time.DateTime.add{Hours,Minutes,Seconds,Milliseconds}"
        [ test "can add a positive number of hours" <|
            \() ->
                epoch
                    |> addHours 26
                    |> toTuple
                    |> Expect.equal ( 1970, 1, 2, 2, 0, 0, 0 )
        , test "can add a negative number of hours" <|
            \() ->
                epoch
                    |> addHours -24
                    |> toTuple
                    |> Expect.equal ( 1969, 12, 31, 0, 0, 0, 0 )
        , test "can add a positive number of minutes" <|
            \() ->
                epoch
                    |> addMinutes 5
                    |> toTuple
                    |> Expect.equal ( 1970, 1, 1, 0, 5, 0, 0 )
        , test "can add a negative number of minutes" <|
            \() ->
                epoch
                    |> addMinutes -1
                    |> toTuple
                    |> Expect.equal ( 1969, 12, 31, 23, 59, 0, 0 )
        , test "can add a positive number of seconds" <|
            \() ->
                epoch
                    |> addSeconds 30
                    |> toTuple
                    |> Expect.equal ( 1970, 1, 1, 0, 0, 30, 0 )
        , test "can add a positive number of seconds as an absolute quantity" <|
            \() ->
                epoch
                    |> addSeconds 3600
                    |> toTuple
                    |> Expect.equal ( 1970, 1, 1, 1, 0, 0, 0 )
        , test "can add a negative number of seconds" <|
            \() ->
                epoch
                    |> addSeconds -1
                    |> toTuple
                    |> Expect.equal ( 1969, 12, 31, 23, 59, 59, 0 )
        , test "can add a positive number of milliseconds" <|
            \() ->
                epoch
                    |> addMilliseconds 30
                    |> toTuple
                    |> Expect.equal ( 1970, 1, 1, 0, 0, 0, 30 )
        , test "can add a positive number of milliseconds as an absolute quantity" <|
            \() ->
                epoch
                    |> addMilliseconds 86400000
                    |> toTuple
                    |> Expect.equal ( 1970, 1, 2, 0, 0, 0, 0 )
        , test "can add a negative number of milliseconds" <|
            \() ->
                epoch
                    |> addMilliseconds -1
                    |> toTuple
                    |> Expect.equal ( 1969, 12, 31, 23, 59, 59, 999 )
        ]


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
        describe "Time.DateTime.{to,from}ISO8601"
            [ test "toISO8601 of epoch is correct" <|
                \() -> toISO8601 epoch |> Expect.equal "1970-01-01T00:00:00Z"
            , test "fromISO8601 is the inverse of toISO8601" <|
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
                        ( sign, signStr ) =
                            if hour < 0 then
                                ( -1, "+" )
                            else
                                ( 1, "-" )

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
                                |> Maybe.map (addMinutes <| sign * minute)
                    in
                        parseEq input output
            ]


all : Test
all =
    describe "Time.DateTime"
        [ dateTimes
        , setters
        , addition
        , toFromISO8601
        ]
