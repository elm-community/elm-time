module TestDateTime exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, oneOf)
import Test exposing (..)
import Time.Date as Date
import Time.DateTime as DateTime exposing (..)


posInt : Fuzzer Int
posInt =
    Fuzz.map abs int


dateTimesEqual : DateTime -> ( Int, Int, Int, Int, Int, Int, Int ) -> Expect.Expectation
dateTimesEqual dateTime dateTimeTuple =
    Expect.equal (toTuple dateTime) dateTimeTuple


dateTimes : Test
dateTimes =
    describe "Time.DateTime.{dateTime,epoch}"
        [ test "epoch is the epoch" <|
            \() ->
                Expect.equal
                    epoch
                    (dateTime { zero | year = 1970 })
        , test "can construt valid dates" <|
            \() ->
                let
                    (( year, month, day, hour, minute, second, millisecond ) as dateTimeTuple) =
                        ( 1992, 5, 29, 23, 29, 50, 920 )
                in
                    dateTimesEqual
                        (dateTime { year = year, month = month, day = day, hour = hour, minute = minute, second = second, millisecond = millisecond })
                        dateTimeTuple
        , test "clamps invalid dates" <|
            \() ->
                dateTimesEqual
                    (dateTime { zero | year = 1993, month = 2, day = 29 })
                    ( 1993, 2, 28, 0, 0, 0, 0 )
        ]


setters : Test
setters =
    describe "Time.DateTime.set{Year,Month,Day,Hour,Minute,Second,Millisecond}"
        [ test "can set year" <|
            \() ->
                dateTime zero
                    |> setYear 1992
                    |> year
                    |> Expect.equal 1992
        , test "can set month" <|
            \() ->
                dateTime zero
                    |> setMonth 5
                    |> month
                    |> Expect.equal 5
        , test "can set month given the current year and day" <|
            \() ->
                dateTime { zero | year = 1992, month = 1, day = 29 }
                    |> setMonth 2
                    |> month
                    |> Expect.equal 2
        , test "can set day" <|
            \() ->
                dateTime zero
                    |> setDay 15
                    |> day
                    |> Expect.equal 15
        , test "can set day given the current year and month" <|
            \() ->
                dateTime { zero | year = 1992, month = 2 }
                    |> setDay 29
                    |> day
                    |> Expect.equal 29
        , test "can set hour" <|
            \() ->
                dateTime zero
                    |> setHour 23
                    |> hour
                    |> Expect.equal 23
        , test "can set minute" <|
            \() ->
                dateTime zero
                    |> setMinute 23
                    |> minute
                    |> Expect.equal 23
        , test "can set second" <|
            \() ->
                dateTime zero
                    |> setSecond 23
                    |> second
                    |> Expect.equal 23
        , test "can set millisecond" <|
            \() ->
                dateTime zero
                    |> setMillisecond 230
                    |> millisecond
                    |> Expect.equal 230
        , test "invalid months are clamped" <|
            \() ->
                dateTime zero
                    |> setMonth 31
                    |> month
                    |> Expect.equal 12
        , test "invalid days are clamped" <|
            \() ->
                dateTime zero
                    |> setDay 105
                    |> day
                    |> Expect.equal 31
        , test "invalid months given the current year and day are clamped to the nearest valid date" <|
            \() ->
                dateTime { zero | year = 1993, month = 1, day = 31 }
                    |> setMonth 2
                    |> date
                    |> Expect.equal (Date.date 1993 2 28)
        , test "invalid days given the current year and month are clamped to the nearest valid date" <|
            \() ->
                dateTime { zero | year = 1991, month = 2 }
                    |> setDay 29
                    |> date
                    |> Expect.equal (Date.date 1991 2 28)
        , fuzz4 int int int int "invalid times are clamped" <|
            \hour minute second millisecond ->
                dateTime zero
                    |> setHour hour
                    |> setMinute minute
                    |> setSecond second
                    |> setMillisecond millisecond
                    |> (\d -> ( DateTime.hour d, DateTime.minute d, DateTime.second d, DateTime.millisecond d ))
                    |> Expect.equal ( clamp 0 23 hour, clamp 0 59 minute, clamp 0 59 second, clamp 0 999 millisecond )
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
                ( Err message, _ ) ->
                    Expect.fail (message ++ " in input '" ++ input ++ "'")

                ( Ok date1, date2 ) ->
                    if date1 == date2 then
                        Expect.pass
                    else
                        Expect.fail ("expected '" ++ toISO8601 date1 ++ "' to equal '" ++ toISO8601 date2 ++ "' from input '" ++ input ++ "'")

        parseFails input =
            case fromISO8601 input of
                Err _ ->
                    Expect.pass

                Ok _ ->
                    Expect.fail ("parsing '" ++ input ++ "' should have failed")

        parseMs input ms =
            case fromISO8601 input of
                Err _ ->
                    Expect.fail "parse failed"

                Ok dt ->
                    Expect.equal (millisecond dt) ms
    in
        describe "Time.DateTime.{to,from}ISO8601"
            [ test "toISO8601 of epoch is correct" <|
                \() ->
                    toISO8601 epoch
                        |> Expect.equal "1970-01-01T00:00:00.000Z"
            , test "fromISO8601 is the inverse of toISO8601" <|
                \() ->
                    toISO8601 epoch
                        |> flip parseEq epoch
            , test "fromISO8601 fails to parse invalid strings" <|
                \() -> parseFails "foo"
            , test "fromISO8601 fails to parse invalid UTC datetimes" <|
                \() -> parseFails "1993-02-29T00:00:00Z"
            , test "fromISO8601 can parse valid UTC datetime strings" <|
                \() -> parseEq "1992-05-29T12:25:12Z" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 12, minute = 25, second = 12 })
            , test "fromISO8601 can parse valid UTC datetime strings including milliseconds" <|
                \() -> parseEq "1992-05-29T12:25:12.001Z" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 12, minute = 25, second = 12, millisecond = 1 })
            , test "fromISO8601 can parse valid UTC datetime strings including zero milliseconds" <|
                \() -> parseEq "1992-05-29T12:25:12.000Z" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 12, minute = 25, second = 12, millisecond = 0 })
            , test "fromISO8601 can parse valid UTC datetime strings including milliseconds and offset" <|
                \() -> parseEq "1992-05-29T12:25:12.001-04:00" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 16, minute = 25, second = 12, millisecond = 1})
            , test "fromISO8601 can parse valid UTC datetime strings including zero milliseconds and offset" <|
                \() -> parseEq "1992-05-29T12:25:12.000-04:00" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 16, minute = 25, second = 12, millisecond = 0})
            , test "fromISO8601 can parse valid fractions in the hundreds" <|
                \() -> parseMs "2016-11-14T03:56:12.123Z" 123
            , test "fromISO8601 can parse valid fractions in the tens" <|
                \() -> parseMs "2016-11-14T03:56:12.12Z" 120
            , test "fromISO8601 can parse valid fractions in the ones" <|
                \() -> parseMs "2016-11-14T03:56:12.1Z" 100
            , test "fromISO8601 can parse valid padded fractions in the tens" <|
                \() -> parseMs "2016-11-14T03:56:12.01Z" 10
            , test "fromISO8601 can parse valid padded fractions in the ones" <|
                \() -> parseMs "2016-11-14T03:56:12.001Z" 1
            , test "fromISO8601 can parse valid padded fractions in the zeros" <|
                \() -> parseMs "2016-11-14T03:56:12.000Z" 0
            , test "fromISO8601 fractions are capped at millisecond precision" <|
                \() -> parseMs "2016-11-14T03:56:12.12345Z" 123
            , test "fromISO8601 fractions are capped at millisecond precision with padding" <|
                \() -> parseMs "2016-11-14T03:56:12.0012345Z" 1
            , test "fromISO8601 fractions are capped at millisecond precision with padding 2" <|
                \() -> parseMs "2016-11-14T03:56:12.0001234Z" 0
            , test "fromISO8601 fractions can be all zeros" <|
                \() -> parseMs "2016-11-14T03:56:12.000Z" 0
            , test "fromISO8601 zero fractions with offsets" <|
                \() -> parseMs "2017-07-03T11:27:11.000-0400" 0
            , test "fromISO8601 fraction 001 with offsets" <|
                \() -> parseMs "2017-07-03T11:27:11.001-0400" 1
            , test "toISO8601 should format 3-digit milliseconds" <|
                \() ->
                    epoch
                        |> setMillisecond 396
                        |> toISO8601
                        |> Expect.equal "1970-01-01T00:00:00.396Z"
            , test "toISO8601 should format 2-digit milliseconds" <|
                \() ->
                    epoch
                        |> setMillisecond 96
                        |> toISO8601
                        |> Expect.equal "1970-01-01T00:00:00.096Z"
            , test "toISO8601 should format 1-digit milliseconds" <|
                \() ->
                    epoch
                        |> setMillisecond 6
                        |> toISO8601
                        |> Expect.equal "1970-01-01T00:00:00.006Z"
            , fuzz4 (intRange -23 23) (intRange 0 59) (oneOf [ constant "-", constant "−" ]) (oneOf [ constant ":", constant "" ]) "fromISO8601 parses offsets correctly" <|
                \hour minute negStr separator ->
                    let
                        ( sign, signStr ) =
                            if hour <= 0 then
                                ( -1, "+" )
                            else
                                ( 1, negStr )

                        padded n =
                            if abs n < 10 then
                                "0" ++ toString (abs n)
                            else
                                toString (abs n)

                        input =
                            "1992-05-29T12:25:12" ++ signStr ++ padded hour ++ separator ++ padded minute

                        output =
                            dateTime { zero | year = 1992, month = 5, day = 29, hour = 12, minute = 25, second = 12 }
                                |> addHours hour
                                |> addMinutes (sign * minute)
                    in
                        parseEq input output
            , fuzz2 (oneOf [ constant "-", constant "" ]) (oneOf [ constant ":", constant "" ]) "fromISO8601 handles basic and extended time/date formats correctly" <|
                \dateSep timeSep ->
                    let
                        input =
                            String.join dateSep [ "1992", "05", "29" ] ++ "T" ++ String.join timeSep [ "12", "25", "12" ] ++ "Z"

                        output =
                            dateTime { zero | year = 1992, month = 5, day = 29, hour = 12, minute = 25, second = 12 }
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
