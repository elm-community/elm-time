module TestZonedDateTime exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Time.DateTime as DT
import Time.TimeZones exposing (europe_bucharest)
import Time.ZonedDateTime exposing (..)


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
