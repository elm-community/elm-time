module TestZonedDateTime exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Time.DateTime as DT
import Time.TimeZones exposing (europe_bucharest)
import Time.ZonedDateTime exposing (..)


summerTimes : Test
summerTimes =
    describe "Time.DateTime.{zonedDateTime,fromDateTime}"
        [ test "Zoned from DateTime applies correct offset before the end of summer time" <|
            \() ->
                DT.dateTime { zero | year = 2016, month = 10, day = 29, hour = 23, minute = 59 }
                    |> fromDateTime (europe_bucharest ())
                    |> toISO8601
                    |> Expect.equal "2016-10-30T02:59:00+03:00"
        , test "Zoned from DateTime applies correct offset during the end of summer time" <|
            \() ->
                DT.dateTime { zero | year = 2016, month = 10, day = 30, hour = 0 }
                    |> fromDateTime (europe_bucharest ())
                    |> toISO8601
                    |> Expect.equal "2016-10-30T03:00:00+02:00"
        , test "Zoned from DateTime applies correct offset after the end of summer time" <|
            \() ->
                DT.dateTime { zero | year = 2016, month = 10, day = 30, hour = 1 }
                    |> fromDateTime (europe_bucharest ())
                    |> toISO8601
                    |> Expect.equal "2016-10-30T03:00:00+02:00"
        , test "Zoned constructor applies correct offset before the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 2 }
                    |> toISO8601
                    |> Expect.equal "2016-10-30T02:00:00+03:00"
        , test "Zoned constructor applies correct offset during the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 3 }
                    |> toISO8601
                    |> Expect.equal "2016-10-30T03:00:00+02:00"
        , test "Zoned constructor applies correct offset after the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 4 }
                    |> toISO8601
                    |> Expect.equal "2016-10-30T04:00:00+02:00"
        ]


convertingToDateTime : Test
convertingToDateTime =
    describe "Time.DateTime.toDateTime"
        [ test "DateTime to Zoned and back produces correct result" <|
            \() ->
                let
                    dt =
                        DT.dateTime { zero | year = 2016, month = 10, day = 30, hour = 1, minute = 30 }
                in
                    dt
                        |> fromDateTime (europe_bucharest ())
                        |> toDateTime
                        |> Expect.equal dt
        , test "DateTime from naive Zoned produces correct result" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 1, minute = 30 }
                    |> toDateTime
                    |> Expect.equal (DT.dateTime { zero | year = 2016, month = 10, day = 29, hour = 22, minute = 30 })
        , test "DateTime from naive Zoned produces correct result during the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 3 }
                    |> toDateTime
                    |> Expect.equal (DT.dateTime { zero | year = 2016, month = 10, day = 30, hour = 1 })
        , test "DateTime from naive Zoned produces correct result after the end of summer time" <|
            \() ->
                zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 10, day = 30, hour = 4 }
                    |> toDateTime
                    |> Expect.equal (DT.dateTime { zero | year = 2016, month = 10, day = 30, hour = 2 })
        ]


all : Test
all =
    describe "Time.ZonedDateTime"
        [ summerTimes
        , convertingToDateTime
        ]
