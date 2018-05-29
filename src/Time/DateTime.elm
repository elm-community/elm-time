module Time.DateTime
    exposing
        ( DateTime
        , DateTimeDelta
        , addDays
        , addHours
        , addMilliseconds
        , addMinutes
        , addMonths
        , addSeconds
        , addYears
        , compare
        , date
        , dateTime
        , day
        , delta
        , epoch
        , fromTimestamp
        , fromTuple
        , hour
        , isValidTime
        , millisecond
        , minute
        , makeDateTime
        , month
        , second
        , setDate
        , setDay
        , setHour
        , setMillisecond
        , setMinute
        , setMonth
        , setSecond
        , setYear
        , toISO8601
        , toTimestamp
        , toTuple
        , weekday
        , year
        , zero
        )

{-| This module defines a time representation based on a Date and the
time of day.


# DateTimes

@docs DateTime, zero, epoch, dateTime, date, year, month, day, weekday, hour, minute, second, millisecond


# Manipulating DateTimes

@docs makeDateTime, setDate, setYear, setMonth, setDay, setHour, setMinute, setSecond, setMillisecond, addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMilliseconds


# Comparing DateTimes

@docs compare


# Subtracting DateTimes

@docs DateTimeDelta, delta


# Helper functions

@docs isValidTime, toTimestamp, fromTimestamp, toTuple, fromTuple


# Deprecated

@docs toISO8601

-}

import Time exposing (Time)
import Time.Date exposing (Date, Weekday, isValidDate)
import Time.Internal exposing (..)


{-| DateTime is the opaque type for all DateTime values. Values of this
type represent a valid Date and a time offset from midnight.
-}
type DateTime
    = DateTime
        { date : Date
        , offset : Int
        }


{-| DateTimeDelta represents the relative difference between two
DateTime values.
-}
type alias DateTimeDelta =
    { years : Int
    , months : Int
    , days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    , milliseconds : Int
    }


{-| zero represents the first millisecond of the first day of the
current era. Use it to construct `DateTime` values:

    dateTime zero
    |> toISO8601
    --> "0-01-01T00:00:00.000Z"

    dateTime { zero | year = 2016 }
    |> toISO8601
    --> "2016-01-01T00:00:00.000Z"

    dateTime { zero | year = 2016, month = 5, day = 29, hour = 13 }
    |> toISO8601
    --> "2016-05-29T13:00:00.000Z"

-}
zero : DateTimeData
zero =
    Time.Internal.zero


{-| epoch is the instant in time that represents the first millisecond
of the UNIX Epoch.

    epoch
    |> toISO8601
    --> "1970-01-01T00:00:00.000Z"

-}
epoch : DateTime
epoch =
    dateTime { zero | year = 1970 }


{-| dateTime constructs a DateTime value given a date and a time.
Invalid values are clamped to the nearest valid date and time.

    import Time.Date

    dateTime { year = 2018
             , month = 13   -- will be clamped
             , day = 25
             , hour = 0
             , minute = 0
             , second = 0
             , millisecond = 47
             }
    |> date
    --> Time.Date.date 2018 12 25

    dateTime { year = 2018
             , month = 13
             , day = 25
             , hour = 0
             , minute = 0
             , second = 0
             , millisecond = 47
             }
    |> millisecond
    --> 47

-}
dateTime : DateTimeData -> DateTime
dateTime ({ year, month, day } as data) =
    DateTime
        { date = Time.Date.date year month day
        , offset = offsetFromTimeData data
        }


mkDateTime : Date -> TimeData d -> DateTime
mkDateTime date time =
    DateTime { date = date, offset = offsetFromTimeData time }


{-| Create a DateTime given its date and millisecond offset

    import Time.Date

    makeDateTime (Time.Date.date 2018 13 26) 1047
    |> date
    --> Time.Date.date 2018 13 26

    makeDateTime (Time.Date.date 2018 13 26) 1047
    |> millisecond
    --> 47

    makeDateTime (Time.Date.date 2018 13 26) 1047
    |> second
    --> 1

-}
makeDateTime : Date -> Int -> DateTime
makeDateTime date offset =
    DateTime { date = date, offset = offset }


{-| date returns a DateTime's Date.

    import Time.Date

    dateTime { year = 2018
             , month = 0   -- will be clamped
             , day = 25
             , hour = 0
             , minute = 0
             , second = 0
             , millisecond = 47
             }
    |> date
    --> Time.Date.date 2018 1 25

-}
date : DateTime -> Date
date (DateTime { date }) =
    date


{-| year returns a DateTime's year.

    dateTime { zero | year = 2015 }
    |> year
    --> 2015

-}
year : DateTime -> Int
year (DateTime { date }) =
    Time.Date.year date


{-| month returns a DateTime's month.

    dateTime { zero | month = 7 }
    |> month
    --> 7

    dateTime { zero | month = 0 } -- will be clamped
    |> month
    --> 1

-}
month : DateTime -> Int
month (DateTime { date }) =
    Time.Date.month date


{-| day returns a DateTime's day.

    dateTime { zero | day = 31 }
    |> day
    --> 31

    dateTime { zero | day = 32 } -- will be clamped
    |> day
    --> 31

-}
day : DateTime -> Int
day (DateTime { date }) =
    Time.Date.day date


{-| weekday returns a DateTime's day of the week.

    import Time.Date

    dateTime { zero | year = 2018, month = 5, day = 27 }
    |> weekday
    --> Time.Date.Sun

-}
weekday : DateTime -> Weekday
weekday (DateTime { date }) =
    Time.Date.weekday date


{-| hour returns a DateTime's hour.

    dateTime { zero | hour = 23 }
    |> hour
    --> 23

    dateTime { zero | hour = 24 } -- will be clamped
    |> hour
    --> 23

-}
hour : DateTime -> Int
hour (DateTime { offset }) =
    offset // hourMs


{-| minute returns a DateTime's minute.

    dateTime { zero | minute = 59 }
    |> minute
    --> 59

    dateTime { zero | minute = 60 } -- will be clamped
    |> minute
    --> 59

-}
minute : DateTime -> Int
minute (DateTime { offset }) =
    (offset % hourMs) // minuteMs


{-| second returns a DateTime's second.

    dateTime { zero | second = 59 }
    |> second
    --> 59

    dateTime { zero | second = 60 } -- will be clamped
    |> second
    --> 59

-}
second : DateTime -> Int
second (DateTime { offset }) =
    (offset % hourMs % minuteMs) // secondMs


{-| millisecond returns a DateTime's millisecond.

    dateTime { zero | millisecond = 999 }
    |> millisecond
    --> 999

    dateTime { zero | millisecond = 1000 } -- will be clamped
    |> millisecond
    --> 999

-}
millisecond : DateTime -> Int
millisecond (DateTime { offset }) =
    offset % hourMs % minuteMs % secondMs


{-| setDate sets a DateTime's Date.

    import Time.Date as TD

    dateTime zero
    |> setDate (TD.date 2018 5 27)
    |> date
    --> TD.date 2018 5 27

-}
setDate : Date -> DateTime -> DateTime
setDate date (DateTime { offset }) =
    DateTime
        { date = date
        , offset = offset
        }


{-| setYear sets a DateTime's year.

See also `Time.Date.setYear`.

    dateTime zero
    |> setYear 2018
    |> year
    --> 2018

-}
setYear : Int -> DateTime -> DateTime
setYear year (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.setYear year date
        , offset = offset
        }


{-| setMonth sets a DateTime's month.

See also `Time.Date.setMonth`.

    dateTime zero
    |> setMonth 12
    |> month
    --> 12

-}
setMonth : Int -> DateTime -> DateTime
setMonth month (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.setMonth month date
        , offset = offset
        }


{-| setDay sets a DateTime's day.

See also `Time.Date.setDay`.

    dateTime zero
    |> setDay 31
    |> day
    --> 31

-}
setDay : Int -> DateTime -> DateTime
setDay day (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.setDay day date
        , offset = offset
        }


{-| setHour sets a DateTime's hour.

    dateTime zero
    |> setHour 23
    |> hour
    --> 23

-}
setHour : Int -> DateTime -> DateTime
setHour hour ((DateTime { date }) as t) =
    mkDateTime date
        { hour = hour
        , minute = minute t
        , second = second t
        , millisecond = millisecond t
        }


{-| setMinute sets a DateTime's minute.

    dateTime zero
    |> setMinute 59
    |> minute
    --> 59

-}
setMinute : Int -> DateTime -> DateTime
setMinute minute ((DateTime { date }) as t) =
    mkDateTime date
        { hour = hour t
        , minute = minute
        , second = second t
        , millisecond = millisecond t
        }


{-| setSecond sets a DateTime's second.
dateTime zero
|> setSecond 59
|> second
--> 59
-}
setSecond : Int -> DateTime -> DateTime
setSecond second ((DateTime { date }) as t) =
    mkDateTime date
        { hour = hour t
        , minute = minute t
        , second = second
        , millisecond = millisecond t
        }


{-| setMillisecond sets a DateTime's millisecond.

    dateTime zero
    |> setMillisecond 999
    |> millisecond
    --> 999

-}
setMillisecond : Int -> DateTime -> DateTime
setMillisecond millisecond ((DateTime { date }) as t) =
    mkDateTime date
        { hour = hour t
        , minute = minute t
        , second = second t
        , millisecond = millisecond
        }


{-| addYears adds a relative number of years to a DateTime value.

See also `Time.Date.addYears`.

    dateTime { zero | year = 2016 }
    |> addYears 2
    |> year
    --> 2018

-}
addYears : Int -> DateTime -> DateTime
addYears years (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.addYears years date
        , offset = offset
        }


{-| addMonths adds a relative number of months to a DateTime value.

See also `Time.Date.addMonths`.

    dateTime { zero | month = 1 }
    |> addMonths 1
    |> month
    --> 2

-}
addMonths : Int -> DateTime -> DateTime
addMonths months (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.addMonths months date
        , offset = offset
        }


{-| addDays adds a relative number of days to a DateTime value.

See also `Time.Date.addDays`.

    dateTime { zero | day = 20 }
    |> addDays -11
    |> day
    --> 9

-}
addDays : Int -> DateTime -> DateTime
addDays days (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.addDays days date
        , offset = offset
        }


{-| addHours adds a relative number of hours to a DateTime value.

    dateTime { zero | hour = 23 }
    |> addHours 1
    |> hour
    --> 0

-}
addHours : Int -> DateTime -> DateTime
addHours hours time =
    addMilliseconds (hours * hourMs) time


{-| addMinutes adds a relative number of minutes to a DateTime value.

    dateTime { zero | minute = 30 }
    |> addMinutes 30
    |> minute
    --> 0

-}
addMinutes : Int -> DateTime -> DateTime
addMinutes minutes time =
    addMilliseconds (minutes * minuteMs) time


{-| addSeconds adds a relative number of seconds to a DateTime value.

    dateTime { zero | second = 59 }
    |> addSeconds 1
    |> second
    --> 0

-}
addSeconds : Int -> DateTime -> DateTime
addSeconds seconds time =
    addMilliseconds (seconds * secondMs) time


{-| addMilliseconds adds an absolute number of milliseconds to a
DateTime value.

    dateTime { zero | second = 10, millisecond = 1 }
    |> addMilliseconds 999
    |> second
    --> 11

-}
addMilliseconds : Int -> DateTime -> DateTime
addMilliseconds ms (DateTime { date, offset }) =
    let
        total =
            ms + offset

        ( days, newOffset ) =
            if total < 0 then
                let
                    days =
                        -(abs total // dayMs + 1)

                    offset =
                        rem total dayMs
                in
                    if offset == 0 then
                        ( days + 1, 0 )
                    else
                        ( days, dayMs + rem offset dayMs )
            else
                ( total // dayMs, rem total dayMs )
    in
        DateTime
            { date = Time.Date.addDays days date
            , offset = newOffset
            }


{-| compare two DateTimes.

    import Basics exposing (Order(..))

    epoch
    |> addMilliseconds -1
    |> Time.DateTime.compare (dateTime
                               { year = 1969
                               , month = 12
                               , day = 31
                               , hour = 23
                               , minute = 59
                               , second = 59
                               , millisecond = 999
                               }
                             )
    --> EQ

-}
compare : DateTime -> DateTime -> Order
compare dt1 dt2 =
    -- comparison of 7-tuples is not supported so we use toISO8601 instead
    Basics.compare (toISO8601 dt1) (toISO8601 dt2)


{-| delta computes the relative difference between two DateTime values.
See also `Time.Date.delta`.

    upper : DateTime
    upper = dateTime
        { year = 1970
        , month = 1
        , day = 1
        , hour = 0
        , minute = 0
        , second = 0
        , millisecond = 0
        }

    upper
    |> addYears -1
    |> delta upper
    --> { years = 1
    --> , months = 12
    --> , days = 365
    --> , hours = 8760
    --> , minutes = 525600
    --> , seconds = 31536000
    --> , milliseconds = 31536000000
    --> }

-}
delta : DateTime -> DateTime -> DateTimeDelta
delta (DateTime t1) (DateTime t2) =
    let
        { years, months, days } =
            Time.Date.delta t1.date t2.date

        milliseconds =
            days * dayMs + (t1.offset - t2.offset)

        hours =
            milliseconds // hourMs

        minutes =
            milliseconds // minuteMs

        seconds =
            milliseconds // secondMs
    in
        { years = years
        , months = months
        , days = days
        , hours = hours
        , minutes = minutes
        , seconds = seconds
        , milliseconds = milliseconds
        }


{-| isValidTime returns True if the given hour, minute, second and
millisecond represent a valid time of day.

    isValidTime 24 0 0 0
    --> False

    isValidTime 23 59 59 999
    --> True

-}
isValidTime : Int -> Int -> Int -> Int -> Bool
isValidTime hour minute second millisecond =
    hour >= 0 && hour < 24 && minute >= 0 && minute < 60 && second >= 0 && second < 60 && millisecond >= 0 && millisecond < 1000


{-| toTimestamp converts a DateTime value to its UNIX timestamp
representation as milliseconds.

    epoch
    |> toTimestamp
    --> 0.0

-}
toTimestamp : DateTime -> Time
toTimestamp time =
    delta time epoch
        |> .milliseconds
        |> toFloat


{-| fromTimestamp converts the millisecond representation of a
UNIX timestamp into a DateTime value.

    fromTimestamp 0.0
    --> epoch

-}
fromTimestamp : Time -> DateTime
fromTimestamp timestamp =
    addMilliseconds (round timestamp) epoch


{-| toTuple converts a DateTime into a (year, month, day, hour, miunte,
second, millisecond) tuple.

DEPRECATED - unavailable in Elm 0.19

    toTuple epoch
    --> (1970, 1, 1, 0, 0, 0, 0)

-}
toTuple : DateTime -> ( Int, Int, Int, Int, Int, Int, Int )
toTuple ((DateTime { date }) as t) =
    let
        ( year, month, day ) =
            Time.Date.toTuple date
    in
        ( year, month, day, hour t, minute t, second t, millisecond t )


{-| fromTuple converts a (year, month, day, hour, minute, second,
millisecond) tuple into a DateTime.

DEPRECATED - unavailable in Elm 0.19

    fromTuple (1970, 1, 1, 0, 0, 0, 0)
    --> epoch

-}
fromTuple : ( Int, Int, Int, Int, Int, Int, Int ) -> DateTime
fromTuple ( year, month, day, hour, minute, second, millisecond ) =
    dateTime
        { year = year
        , month = month
        , day = day
        , hour = hour
        , minute = minute
        , second = second
        , millisecond = millisecond
        }


{-| toISO8601 renders a DateTime in ISO8601 format.

DEPRECATED: this is used as a hack for the compare function above; it is not exposed --
use the functionality in Iso8601 instead.

-}
toISO8601 : DateTime -> String
toISO8601 time =
    toString (year time)
        ++ "-"
        ++ padded (month time)
        ++ "-"
        ++ padded (day time)
        ++ "T"
        ++ padded (hour time)
        ++ ":"
        ++ padded (minute time)
        ++ ":"
        ++ padded (second time)
        ++ "."
        ++ padded3 (millisecond time)
        ++ "Z"
