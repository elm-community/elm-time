module TestDateTime exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, oneOf)
import Parser exposing (Context, Error, Parser, Problem)
import Test exposing (..)
import Time.Date as Date
import Time.DateTime as DateTime
    exposing
        ( DateTime
        , addHours
        , addMilliseconds
        , addMinutes
        , addSeconds
        , date
        , dateTime
        , day
        , epoch
        , hour
        , millisecond
        , minute
        , month
        , second
        , setDay
        , setHour
        , setMillisecond
        , setMinute
        , setMonth
        , setSecond
        , setYear
        , toTuple
        , year
        , zero
        )
import Time.Iso8601 as Iso8601


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
