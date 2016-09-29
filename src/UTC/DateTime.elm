module UTC.DateTime
    exposing
        ( DateTime
        , DateTimeDelta
        , dateTime
        , epoch
        , date
        , year
        , month
        , day
        , hour
        , minute
        , second
        , millisecond
        , setDate
        , setYear
        , setMonth
        , setDay
        , setHour
        , setMinute
        , setSecond
        , setMillisecond
        , addYears
        , addMonths
        , addDays
        , addHours
        , addMinutes
        , addSeconds
        , addMilliseconds
        , delta
        , toTimestamp
        , fromTimestamp
        , toTuple
        , fromTuple
        , toISO8601
        )

{-| This module defines a time representation based on a Date and the
time of day.

# DateTimes
@docs DateTime, epoch, dateTime, date, year, month, day, hour, minute, second, millisecond

# Manipulating DateTimes
@docs setDate, setYear, setMonth, setDay, setHour, setMinute, setSecond, setMillisecond, addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMilliseconds

# Subtracting DateTimes
@docs DateTimeDelta, delta

# Helper functions
@docs toTimestamp, fromTimestamp, toTuple, fromTuple, toISO8601
-}

import Calendar.Date exposing (Date)
import Time exposing (Time)
import UTC.Internal exposing (..)


{-| DateTime is the opaque type for all DateTime values.  Values of this
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


{-| dateTime constructs a DateTime value give a date, an hour, a minute,
a second and a millisecond.  If the constructed value is invalid,
Nothing is returned.
-}
dateTime : Date -> Int -> Int -> Int -> Int -> Maybe DateTime
dateTime date hour minute second millisecond =
    if hour >= 0 && hour < 24 && minute >= 0 && minute < 60 && second >= 0 && second < 60 && millisecond >= 0 && millisecond < 1000 then
        Just <|
            DateTime
                { date = date
                , offset =
                    (hour * hourMs)
                        + (minute * minuteMs)
                        + (second * secondMs)
                        + millisecond
                }
    else
        Nothing


{-| epoch is the instant in time that represents the first millisecond
of the UNIX Epoch.
-}
epoch : DateTime
epoch =
    let
        date =
            case Calendar.Date.date 1970 1 1 of
                Nothing ->
                    Debug.crash "epoch: failed to construct epoch date"

                Just d ->
                    d
    in
        DateTime { date = date, offset = 0 }


{-| date returns a DateTime's Date.
-}
date : DateTime -> Date
date (DateTime { date }) =
    date


{-| year returns a DateTime's year.
-}
year : DateTime -> Int
year (DateTime { date }) =
    Calendar.Date.year date


{-| month returns a DateTime's month.
-}
month : DateTime -> Int
month (DateTime { date }) =
    Calendar.Date.month date


{-| day returns a DateTime's day.
-}
day : DateTime -> Int
day (DateTime { date }) =
    Calendar.Date.day date


{-| hour returns a DateTime's hour.
-}
hour : DateTime -> Int
hour (DateTime { offset }) =
    offset // hourMs


{-| minute returns a DateTime's minute.
-}
minute : DateTime -> Int
minute (DateTime { offset }) =
    (offset `rem` hourMs) // minuteMs


{-| second returns a DateTime's second.
-}
second : DateTime -> Int
second (DateTime { offset }) =
    (offset `rem` hourMs `rem` minuteMs) // secondMs


{-| millisecond returns a DateTime's millisecond.
-}
millisecond : DateTime -> Int
millisecond (DateTime { offset }) =
    offset `rem` hourMs `rem` minuteMs `rem` secondMs


{-| setDate sets a DateTime's Date.
-}
setDate : Date -> DateTime -> DateTime
setDate date (DateTime { offset }) =
    DateTime
        { date = date
        , offset = offset
        }


{-| setYear sets a DateTime's year, returning Nothing if the updated
time is invalid or Just the new DateTime.

See also `Calendar.Date.setYear`.
-}
setYear : Int -> DateTime -> Maybe DateTime
setYear year (DateTime { date, offset }) =
    Calendar.Date.setYear year date
        |> Maybe.map (\d -> DateTime { date = d, offset = offset })


{-| setMonth sets a DateTime's month, returning Nothing if the updated
time is invalid or Just the new DateTime.

See also `Calendar.Date.setMonth`.
-}
setMonth : Int -> DateTime -> Maybe DateTime
setMonth month (DateTime { date, offset }) =
    Calendar.Date.setMonth month date
        |> Maybe.map (\d -> DateTime { date = d, offset = offset })


{-| setDay sets a DateTime's day, returning Nothing if the updated
time is invalid or Just the new DateTime.

See also `Calendar.Date.setDay`.
-}
setDay : Int -> DateTime -> Maybe DateTime
setDay day (DateTime { date, offset }) =
    Calendar.Date.setDay day date
        |> Maybe.map (\d -> DateTime { date = d, offset = offset })


{-| setHour sets a DateTime's hour, returning Nothing if the updated
time is invalid or Just the new DateTime.
-}
setHour : Int -> DateTime -> Maybe DateTime
setHour hour ((DateTime { date }) as t) =
    dateTime date hour (minute t) (second t) (millisecond t)


{-| setMinute sets a DateTime's minute, returning Nothing if the
updated time is invalid or Just the new DateTime.
-}
setMinute : Int -> DateTime -> Maybe DateTime
setMinute minute ((DateTime { date }) as t) =
    dateTime date (hour t) minute (second t) (millisecond t)


{-| setSecond sets a DateTime's second, returning Nothing if the
updated time is invalid or Just the new DateTime.
-}
setSecond : Int -> DateTime -> Maybe DateTime
setSecond second ((DateTime { date }) as t) =
    dateTime date (hour t) (minute t) second (millisecond t)


{-| setMillisecond sets a DateTime's millisecond, returning Nothing if
the updated time is invalid or Just the new DateTime.
-}
setMillisecond : Int -> DateTime -> Maybe DateTime
setMillisecond millisecond ((DateTime { date }) as t) =
    dateTime date (hour t) (minute t) (second t) millisecond


{-| addYears adds a relative number of years to a DateTime value.

See also `Calendar.Date.addYears`.
-}
addYears : Int -> DateTime -> DateTime
addYears years (DateTime { date, offset }) =
    DateTime
        { date = Calendar.Date.addYears years date
        , offset = offset
        }


{-| addMonths adds a relative number of months to a DateTime value.

See also `Calendar.Date.addMonths`.
-}
addMonths : Int -> DateTime -> DateTime
addMonths months (DateTime { date, offset }) =
    DateTime
        { date = Calendar.Date.addMonths months date
        , offset = offset
        }


{-| addDays adds an absolute number of days to a DateTime value.

See also `Calendar.Date.addDays`.
-}
addDays : Int -> DateTime -> DateTime
addDays days (DateTime { date, offset }) =
    DateTime
        { date = Calendar.Date.addDays days date
        , offset = offset
        }


{-| addHours adds a relative number of hours to a DateTime value.
-}
addHours : Int -> DateTime -> DateTime
addHours hours time =
    addMilliseconds (hours * hourMs) time


{-| addMinutes adds a relative number of minutes to a DateTime value.
-}
addMinutes : Int -> DateTime -> DateTime
addMinutes minutes time =
    addMilliseconds (minutes * minuteMs) time


{-| addSeconds adds a relative number of seconds to a DateTime value.
-}
addSeconds : Int -> DateTime -> DateTime
addSeconds seconds time =
    addMilliseconds (seconds * secondMs) time


{-| addMilliseconds adds an absolute number of milliseconds to a
DateTime value.
-}
addMilliseconds : Int -> DateTime -> DateTime
addMilliseconds ms (DateTime { date, offset }) =
    let
        total =
            ms + offset

        ( days, offset' ) =
            if total < 0 then
                let
                    days =
                        -(abs total // dayMs + 1)

                    offset =
                        total `rem` dayMs
                in
                    if offset == 0 then
                        ( days + 1, 0 )
                    else
                        ( days, dayMs + offset `rem` dayMs )
            else
                ( total // dayMs, total `rem` dayMs )
    in
        DateTime
            { date = Calendar.Date.addDays days date
            , offset = offset'
            }


{-| delta computes the relative difference between two DateTime values.
-}
delta : DateTime -> DateTime -> DateTimeDelta
delta (DateTime t1) (DateTime t2) =
    let
        { years, months, days } =
            Calendar.Date.delta t1.date t2.date

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


{-| toTimestamp converts a DateTime value to its UNIX timestamp
representation as milliseconds.
-}
toTimestamp : DateTime -> Time
toTimestamp time =
    delta time epoch
        |> .milliseconds
        |> toFloat


{-| fromTimestamp converts the millisecond representation of a
UNIX timestamp into a DateTime value.
-}
fromTimestamp : Time -> DateTime
fromTimestamp timestamp =
    addMilliseconds (round timestamp) epoch


{-| toTuple converts a DateTime into a (year, month, day, hour, miunte,
second, millisecond) tuple.
-}
toTuple : DateTime -> ( Int, Int, Int, Int, Int, Int, Int )
toTuple ((DateTime { date }) as t) =
    let
        ( y, m, d ) =
            Calendar.Date.toTuple date
    in
        ( y, m, d, hour t, minute t, second t, millisecond t )


{-| fromTuple converts a (year, month, day, hour, minute, second,
millisecond) tuple into a DateTime.  If the tuple represents an invalid
DateTime then Nothing is returned.
-}
fromTuple : ( Int, Int, Int, Int, Int, Int, Int ) -> Maybe DateTime
fromTuple ( y, m, d, h, mi, s, ms ) =
    Calendar.Date.date y m d
        `Maybe.andThen` \d -> dateTime d h mi s ms


{-| toISO8601 renders a DateTime in ISO8601 format.
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
        ++ "Z"
