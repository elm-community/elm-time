module TestIso8601 exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, oneOf)
import Test exposing (..)
import TestDate exposing (someDate)
import Time.Date exposing (Date, date)
import Time.DateTime as DT
    exposing
        ( addHours
        , addMinutes
        , dateTime
        , epoch
        , millisecond
        , setMillisecond
        , zero
        )
import Time.Iso8601 as Iso8601
    exposing
        ( fromDate
        , fromDateTime
        , fromZonedDateTime
        , toDate
        , toDateTime
        )
import Time.TimeZones exposing (europe_bucharest)
import Time.ZonedDateTime as ZonedDateTime
    exposing
        ( zonedDateTime
        )


{-| -}
toFromISO8601 : Test
toFromISO8601 =
    let
        parseEq input expectedDateTime =
            case toDateTime input of
                Err error ->
                    toString error
                        |> Expect.fail

                Ok actualDateTime ->
                    if actualDateTime == expectedDateTime then
                        Expect.pass
                    else
                        Expect.fail ("expected '" ++ Iso8601.fromDateTime actualDateTime ++ "' to equal '" ++ Iso8601.fromDateTime expectedDateTime ++ "' from input '" ++ input ++ "'")

        parseEqDate input expectedDate =
            case toDate input of
                Err error ->
                    toString error
                        |> Expect.fail

                Ok actualDate ->
                    if actualDate == expectedDate then
                        Expect.pass
                    else
                        Expect.fail ("expected '" ++ Iso8601.fromDate actualDate ++ "' to equal '" ++ Iso8601.fromDate expectedDate ++ "' from input '" ++ input ++ "'")

        parseFails input =
            case toDateTime input of
                Err _ ->
                    Expect.pass

                Ok _ ->
                    Expect.fail ("parsing '" ++ input ++ "' should have failed")

        parseMs input ms =
            case toDateTime input of
                Err _ ->
                    Expect.fail "parse failed"

                Ok dt ->
                    Expect.equal (millisecond dt) ms
    in
        describe "Time.DateTime.{to,from}ISO8601"
            [ test "toISO8601 of epoch is correct" <|
                \() ->
                    Iso8601.fromDateTime epoch
                        |> Expect.equal "1970-01-01T00:00:00.000Z"
            , test "fromISO8601 is the inverse of toISO8601" <|
                \() ->
                    Iso8601.fromDateTime epoch
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
                \() -> parseEq "1992-05-29T12:25:12.001-04:00" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 16, minute = 25, second = 12, millisecond = 1 })
            , test "fromISO8601 can parse valid UTC datetime strings including zero milliseconds and offset" <|
                \() -> parseEq "1992-05-29T12:25:12.000-04:00" (dateTime { zero | year = 1992, month = 5, day = 29, hour = 16, minute = 25, second = 12, millisecond = 0 })
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
            , test "fromISO8601 of a valid year before 1000 (with dashes) is correct" <|
                \() -> parseEqDate "0301-10-02" (date 301 10 2)
            , test "fromISO8601 of a valid year before 1000 (without dashes) is correct" <|
                \() -> parseEqDate "00011002" (date 1 10 2)
            , test "toISO8601 of a valid year before 1000 is correct" <|
                \() ->
                    Iso8601.fromDate (date 301 10 2)
                        |> Expect.equal "0301-10-02"
            , test "toISO8601 should format 3-digit milliseconds" <|
                \() ->
                    epoch
                        |> setMillisecond 396
                        |> Iso8601.fromDateTime
                        |> Expect.equal "1970-01-01T00:00:00.396Z"
            , test "toISO8601 should format 2-digit milliseconds" <|
                \() ->
                    epoch
                        |> setMillisecond 96
                        |> Iso8601.fromDateTime
                        |> Expect.equal "1970-01-01T00:00:00.096Z"
            , test "toISO8601 should format 1-digit milliseconds" <|
                \() ->
                    epoch
                        |> setMillisecond 6
                        |> Iso8601.fromDateTime
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


toFromDateISO8601 : Test
toFromDateISO8601 =
    let
        renderEq : Date -> String -> () -> Expectation
        renderEq date output () =
            fromDate date
                |> Expect.equal output

        parseEq input date () =
            case ( toDate input, date ) of
                ( Err message, _ ) ->
                    Expect.fail (toString message ++ " in input '" ++ input ++ "'")

                ( Ok date1, date2 ) ->
                    if date1 == date2 then
                        Expect.pass
                    else
                        Expect.fail ("expected '" ++ fromDate date1 ++ "' to equal '" ++ fromDate date2 ++ "' from input '" ++ input ++ "'")

        parseFails input () =
            case toDateTime input of
                Err _ ->
                    Expect.pass

                Ok _ ->
                    Expect.fail ("parsing '" ++ input ++ "' should have failed")
    in
        describe "Time.Date.{to,from}ISO8601"
            [ test "toISO8601 of epoch is correct" <|
                renderEq (date 1970 1 1) "1970-01-01"
            , test "toISO8601 of some date is correct" <|
                renderEq someDate "1992-05-29"
            , test "fromISO8601 of a valid date is correct" <|
                parseEq "1992-12-29" (date 1992 12 29)
            , test "fromISO8601 of a valid padded date is correct" <|
                parseEq "1992-05-29" (date 1992 5 29)
            , test "fromISO8601 of a valid padded date with no delimeters is correct" <|
                parseEq "19920529" (date 1992 5 29)
            , test "fromISO8601 of a badly-formatted date fails" <|
                parseFails ""
            , test "fromISO8601 of a badly-formatted date fails 2" <|
                parseFails "1992-05"
            , test "fromISO8601 of an invalid date fails" <|
                parseFails "1991-02-31"
            ]


summerTimes : Test
summerTimes =
    describe "Time.DateTime.{zonedDateTime,fromDateTime}"
        [ test "Zoned from DateTime applies correct offset before the end of summer time" <|
            \() ->
                DT.dateTime { zero | year = 2016, month = 10, day = 29, hour = 23, minute = 59 }
                    |> ZonedDateTime.fromDateTime (europe_bucharest ())
                    |> fromZonedDateTime
                    |> Expect.equal "2016-10-30T02:59:00.000+03:00"
        , test "Zoned from DateTime applies correct offset during the end of summer time" <|
            \() ->
                DT.dateTime { zero | year = 2016, month = 10, day = 30, hour = 0 }
                    |> ZonedDateTime.fromDateTime (europe_bucharest ())
                    |> fromZonedDateTime
                    |> Expect.equal "2016-10-30T03:00:00.000+02:00"
        , test "Zoned from DateTime applies correct offset after the end of summer time" <|
            \() ->
                DT.dateTime { zero | year = 2016, month = 10, day = 30, hour = 1 }
                    |> ZonedDateTime.fromDateTime (europe_bucharest ())
                    |> fromZonedDateTime
                    |> Expect.equal "2016-10-30T03:00:00.000+02:00"
        , test "Zoned constructor applies correct offset before the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 2 }
                    |> fromZonedDateTime
                    |> Expect.equal "2016-10-30T02:00:00.000+03:00"
        , test "Zoned constructor applies correct offset during the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 3 }
                    |> fromZonedDateTime
                    |> Expect.equal "2016-10-30T03:00:00.000+02:00"
        , test "Zoned constructor applies correct offset after the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 4 }
                    |> fromZonedDateTime
                    |> Expect.equal "2016-10-30T04:00:00.000+02:00"
        , test "toISO8601 should format 3-digit milliseconds" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2017, millisecond = 396 }
                    |> fromZonedDateTime
                    |> Expect.equal "2017-01-01T00:00:00.396+02:00"
        , test "toISO8601 should format 2-digit milliseconds" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2017, millisecond = 96 }
                    |> fromZonedDateTime
                    |> Expect.equal "2017-01-01T00:00:00.096+02:00"
        , test "toISO8601 should format 1-digit milliseconds" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2017, millisecond = 6 }
                    |> fromZonedDateTime
                    |> Expect.equal "2017-01-01T00:00:00.006+02:00"
        ]
