module TestDateTimeDelta exposing (..)

import Time.DateTime as DateTime exposing (..)
import Expect exposing (Expectation)
-- import Fuzz exposing (Fuzzer, constant, int, intRange, oneOf)
import Test exposing (..)


date1 : DateTime -- hmm
date1 = DateTime.fromTuple
    ( 1970
    , 1
    , 1
    , 0
    , 0
    , 0
    , 0
    )


date2 : DateTime
date2 = DateTime.fromTuple
    ( 1969
    , 12
    , 31
    , 23
    , 59
    , 59
    , 999
    )


expectedDelta : DateTimeDelta
expectedDelta =
    { years = 1 -- force one if across a year boundary (1969 -> 1970)
    , months = 1 -- force one if across a month boundary (Dec -> Jan)
    , days = 1 -- force one if across a day boundary (31 -> 1)
    , hours = 0 -- round (ignore hour boundary)
    , minutes = 0 -- round (ignore minute boundary)
    , seconds = 0 -- round (ignore second boundary)
    , milliseconds = 1 -- whatever it is
    }


suite : Test
suite =
    describe "DateTime delta"
        [ test "Exploratory: two dates one millisecond apart at midnight New Years" <|
            \_ ->
                DateTime.delta date1 date2
--                    |> Debug.log "date2 - date1"
                    |> Expect.equal expectedDelta
        ]
