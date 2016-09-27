module UTC.Time
    exposing
        ( UTCTime
        , UTCTimeDelta
        , utcTime
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
        , fromISO8601
        )

{-| This module defines a time representation based on a Date and the
time of day.

# UTCTimes
@docs UTCTime, epoch, utcTime, date, year, month, day, hour, minute, second, millisecond

# Manipulating UTCTimes
@docs setDate, setYear, setMonth, setDay, setHour, setMinute, setSecond, setMillisecond, addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMilliseconds

# Subtracting UTCTimes
@docs UTCTimeDelta, delta

# Helper functions
@docs toTimestamp, fromTimestamp, toTuple, fromTuple, toISO8601, fromISO8601
-}

import Calendar.Date exposing (Date)
import Time exposing (Time)


{-| UTCTime is the opaque type for all UTCTime values.  Values of this
type represent valid Date and a time offset from midnight.
-}
type UTCTime
    = UTCTime
        { date : Date
        , millisecond : Float
        }


{-| UTCTimeDelta represents the relative difference between two
UTCTime values.
-}
type alias UTCTimeDelta =
    { years : Int
    , months : Int
    , days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    , milliseconds : Int
    }


{-| utcTime constructs a UTCTime value give a date, an hour, a minute,
a second and a millisecond.  If the constructed value is invalid,
Nothing is returned.
-}
utcTime : Date -> Int -> Int -> Int -> Int -> Maybe UTCTime
utcTime date hour minute second millisecond =
    if hour >= 0 && hour < 24 && minute >= 0 && minute < 60 && second >= 0 && second < 60 && millisecond >= 0 && millisecond < 1000 then
        Just <|
            UTCTime
                { date = date
                , millisecond =
                    toFloat hour
                        * 3600000
                        + toFloat minute
                        * 60000
                        + toFloat second
                        * 1000
                        + toFloat millisecond
                }
    else
        Nothing


{-| epoch is the instant in time that represents the first millisecond
of the UNIX Epoch.
-}
epoch : UTCTime
epoch =
    let
        date =
            case Calendar.Date.date 1970 1 1 of
                Nothing ->
                    Debug.crash "epoch: failed to construct epoch date"

                Just d ->
                    d
    in
        UTCTime { date = date, millisecond = 0 }


{-| date returns a UTCTime's Date.
-}
date : UTCTime -> Date
date (UTCTime { date }) =
    date


{-| year returns a UTCTime's year.
-}
year : UTCTime -> Int
year (UTCTime { date }) =
    Calendar.Date.year date


{-| month returns a UTCTime's month.
-}
month : UTCTime -> Int
month (UTCTime { date }) =
    Calendar.Date.month date


{-| day returns a UTCTime's day.
-}
day : UTCTime -> Int
day (UTCTime { date }) =
    Calendar.Date.day date


{-| hour returns a UTCTime's hour.
-}
hour : UTCTime -> Int
hour (UTCTime { millisecond }) =
    round millisecond // 3600000


{-| minute returns a UTCTime's minute.
-}
minute : UTCTime -> Int
minute (UTCTime { millisecond }) =
    (round millisecond `rem` 3600000) // 60000


{-| second returns a UTCTime's second.
-}
second : UTCTime -> Int
second (UTCTime { millisecond }) =
    (round millisecond `rem` 3600000 `rem` 60000) // 1000


{-| millisecond returns a UTCTime's millisecond.
-}
millisecond : UTCTime -> Int
millisecond (UTCTime { millisecond }) =
    round millisecond `rem` 3600000 `rem` 60000 `rem` 1000


{-| setDate sets a UTCTime's Date.
-}
setDate : Date -> UTCTime -> UTCTime
setDate date (UTCTime { millisecond }) =
    UTCTime
        { date = date
        , millisecond = millisecond
        }


{-| setYear sets a UTCTime's year, returning Nothing if the updated
time is invalid or Just the new UTCTime.

See also `Calendar.Date.setYear`.
-}
setYear : Int -> UTCTime -> Maybe UTCTime
setYear year (UTCTime { date, millisecond }) =
    Calendar.Date.setYear year date
        |> Maybe.map (\d -> UTCTime { date = d, millisecond = millisecond })


{-| setMonth sets a UTCTime's month, returning Nothing if the updated
time is invalid or Just the new UTCTime.

See also `Calendar.Date.setMonth`.
-}
setMonth : Int -> UTCTime -> Maybe UTCTime
setMonth month (UTCTime { date, millisecond }) =
    Calendar.Date.setMonth month date
        |> Maybe.map (\d -> UTCTime { date = d, millisecond = millisecond })


{-| setDay sets a UTCTime's day, returning Nothing if the updated
time is invalid or Just the new UTCTime.

See also `Calendar.Date.setDay`.
-}
setDay : Int -> UTCTime -> Maybe UTCTime
setDay day (UTCTime { date, millisecond }) =
    Calendar.Date.setDay day date
        |> Maybe.map (\d -> UTCTime { date = d, millisecond = millisecond })


{-| setHour sets a UTCTime's hour, returning Nothing if the updated
time is invalid or Just the new UTCTime.
-}
setHour : Int -> UTCTime -> Maybe UTCTime
setHour hour ((UTCTime { date }) as t) =
    utcTime date hour (minute t) (second t) (millisecond t)


{-| setMinute sets a UTCTime's minute, returning Nothing if the
updated time is invalid or Just the new UTCTime.
-}
setMinute : Int -> UTCTime -> Maybe UTCTime
setMinute minute ((UTCTime { date }) as t) =
    utcTime date (hour t) minute (second t) (millisecond t)


{-| setSecond sets a UTCTime's second, returning Nothing if the
updated time is invalid or Just the new UTCTime.
-}
setSecond : Int -> UTCTime -> Maybe UTCTime
setSecond second ((UTCTime { date }) as t) =
    utcTime date (hour t) (minute t) second (millisecond t)


{-| setMillisecond sets a UTCTime's millisecond, returning Nothing if
the updated time is invalid or Just the new UTCTime.
-}
setMillisecond : Int -> UTCTime -> Maybe UTCTime
setMillisecond millisecond ((UTCTime { date }) as t) =
    utcTime date (hour t) (minute t) (second t) millisecond


{-| addYears adds a relative number of years to a UTCTime value.

See also `Calendar.Date.addYears`.
-}
addYears : Int -> UTCTime -> UTCTime
addYears years (UTCTime { date, millisecond }) =
    UTCTime
        { date = Calendar.Date.addYears years date
        , millisecond = millisecond
        }


{-| addMonths adds a relative number of months to a UTCTime value.

See also `Calendar.Date.addMonths`.
-}
addMonths : Int -> UTCTime -> UTCTime
addMonths months (UTCTime { date, millisecond }) =
    UTCTime
        { date = Calendar.Date.addMonths months date
        , millisecond = millisecond
        }


{-| addDays adds an absolute number of days to a UTCTime value.

See also `Calendar.Date.addDays`.
-}
addDays : Int -> UTCTime -> UTCTime
addDays days (UTCTime { date, millisecond }) =
    UTCTime
        { date = Calendar.Date.addDays days date
        , millisecond = millisecond
        }


{-| addHours adds a relative number of hours to a UTCTime value.
-}
addHours : Int -> UTCTime -> UTCTime
addHours hours time =
    addMilliseconds (hours * 3600000) time


{-| addMinutes adds a relative number of minutes to a UTCTime value.
-}
addMinutes : Int -> UTCTime -> UTCTime
addMinutes minutes time =
    addMilliseconds (minutes * 60000) time


{-| addSeconds adds a relative number of seconds to a UTCTime value.
-}
addSeconds : Int -> UTCTime -> UTCTime
addSeconds seconds time =
    addMilliseconds (seconds * 1000) time


{-| addMilliseconds adds an absolute number of milliseconds to a
UTCTime value.
-}
addMilliseconds : Int -> UTCTime -> UTCTime
addMilliseconds ms (UTCTime { date, millisecond }) =
    let
        millisecond' =
            ms + round millisecond
    in
        UTCTime
            { date = Calendar.Date.addDays (millisecond' // 86400000) date
            , millisecond = toFloat <| millisecond' `rem` 86400000
            }


{-| delta computes the relative difference between two UTCTime values.
-}
delta : UTCTime -> UTCTime -> UTCTimeDelta
delta (UTCTime t1) (UTCTime t2) =
    let
        { years, months, days } =
            Calendar.Date.delta t1.date t2.date

        milliseconds =
            days * 86400000 + round (t1.millisecond - t2.millisecond)

        hours =
            milliseconds // 3600000

        minutes =
            milliseconds // 60000

        seconds =
            milliseconds // 1000
    in
        { years = years
        , months = months
        , days = days
        , hours = hours
        , minutes = minutes
        , seconds = seconds
        , milliseconds = milliseconds
        }


{-| toTimestamp converts a UTCTime value to its UNIX timestamp
representation as milliseconds.
-}
toTimestamp : UTCTime -> Time
toTimestamp time =
    delta time epoch
        |> .milliseconds
        |> toFloat


{-| fromTimestamp converts the millisecond representation of a
UNIX timestamp into a UTCTime value.
-}
fromTimestamp : Time -> UTCTime
fromTimestamp timestamp =
    addMilliseconds (round timestamp) epoch


{-| toTuple converts a UTCTime into a (year, month, day, hour, miunte,
second, millisecond) tuple.
-}
toTuple : UTCTime -> ( Int, Int, Int, Int, Int, Int, Int )
toTuple ((UTCTime { date }) as t) =
    let
        ( y, m, d ) =
            Calendar.Date.toTuple date
    in
        ( y, m, d, hour t, minute t, second t, millisecond t )


{-| fromTuple converts a (year, month, day, hour, minute, second,
millisecond) tuple into a UTCTime.  If the tuple represents an invalid
UTCTime then Nothing is returned.
-}
fromTuple : ( Int, Int, Int, Int, Int, Int, Int ) -> Maybe UTCTime
fromTuple ( y, m, d, h, mi, s, ms ) =
    Calendar.Date.date y m d
        `Maybe.andThen` \d -> utcTime d h mi s ms


{-| toISO8601 renders a UTCTime in ISO8601 format.
-}
toISO8601 : UTCTime -> String
toISO8601 time =
    let
        padded n =
            if n < 10 then
                "0" ++ toString n
            else
                toString n
    in
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


{-| fromISO8601 parses an ISO8601-formatted string into a (UTCTime,
offset) pair.  The offset is represented in minutes.
-}
fromISO8601 : String -> Result String ( UTCTime, Int )
fromISO8601 time =
    Err "FIXME"
