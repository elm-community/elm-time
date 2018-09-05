module TestDateTimeDelta exposing (date1, date2, expectedDelta, suite)

-- import Fuzz exposing (Fuzzer, constant, int, intRange, oneOf)

import Expect exposing (Expectation)
import Test exposing (..)
import Time.DateTime as DateTime exposing (..)


date1 : DateTime



-- hmm


date1 =
    DateTime.dateTime
        { year = 1970
        , month = 1
        , day = 1
        , hour = 0
        , minute = 0
        , second = 0
        , millisecond = 0
        }


date2 : DateTime
date2 =
    DateTime.dateTime
        { year = 1969
        , month = 12
        , day = 31
        , hour = 23
        , minute = 59
        , second = 59
        , millisecond = 999
        }


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
