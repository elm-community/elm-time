module Time.ZonedDateTime
    exposing
        ( ZonedDateTime
        , zero
        , zonedDateTime
        , fromDateTime
        , toDateTime
        , fromTimestamp
        , toTimestamp
        , timeZone
        , asTimeZone
        , year
        , month
        , day
        , weekday
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
        , abbreviation
        , utcOffset
        , utcOffsetString
        , toISO8601
        , fromISO8601
        )

{-| This module defines a time representation based on a Date, the
time of day and a time zone.

ZonedDateTimes should only be used when reasoning about or displaying
`DateTime`s in a user's local time zone.  For this reason, the API
surface of `ZonedDateTimes` is extremely limited.

# ZonedDateTimes
@docs ZonedDateTime

# Constructing ZonedDateTimes
@docs zero, zonedDateTime, fromDateTime, toDateTime, fromTimestamp, toTimestamp

# Inspecting ZonedDateTimes
@docs timeZone, year, month, day, weekday, hour, minute, second, millisecond, abbreviation, utcOffset, utcOffsetString

# Manipulating ZonedDateTimes
@docs asTimeZone, setDate, setYear, setMonth, setDay, setHour, setMinute, setSecond, setMillisecond, addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMilliseconds

# Helper functions
@docs toISO8601, fromISO8601
-}

import Time exposing (Time)
import Time.Date exposing (Date, Weekday)
import Time.DateTime as DateTime exposing (DateTime)
import Time.Internal exposing (..)
import Time.TimeZone as TimeZone exposing (TimeZone)


{-| ZoneDateTime is the opaque type for all ZonedDateTime values.
Values of this type represent a `(TimeZone, DateTime)` pair.
-}
type ZonedDateTime
    = ZonedDateTime
        { timeZone : TimeZone
        , dateTime : DateTime
        }


{-| zero represents the first millisecond of the first day of the
current era.  Use it to build `ZonedDateTime` values:

    -- 0-01-01T00:00:00+02:00
    zonedDateTime (europe_bucharest ()) zero

    -- 2016-01-01T00:00:00+02:00
    zonedDateTime (europe_bucharest ()) { zero | year = 2016 }

    -- 2016-05-29T13:00:00+02:00
    zonedDateTime (europe_bucharest ()) { zero | year = 2016, month = 5, day = 29, hour = 13 }
-}
zero : DateTimeData
zero =
    Time.Internal.zero


{-| zonedDateTime constructs a ZonedDateTime value given a TimeZone, a
date and a time.  Invalid values are clamped to the nearest valid date
and time.
-}
zonedDateTime : TimeZone -> DateTimeData -> ZonedDateTime
zonedDateTime timeZone dateTimeData =
    ZonedDateTime
        { timeZone = timeZone
        , dateTime = DateTime.dateTime dateTimeData
        }


{-| fromDateTime constructs a ZonedDateTime value from a TimeZone and
a DateTime.
-}
fromDateTime : TimeZone -> DateTime -> ZonedDateTime
fromDateTime timeZone dateTime =
    let
        timestamp =
            DateTime.toTimestamp dateTime

        offset =
            TimeZone.offset timestamp timeZone
    in
        ZonedDateTime
            { timeZone = timeZone
            , dateTime = DateTime.addMilliseconds -offset dateTime
            }


{-| toDateTime converts a ZonedDateTime to a UTC DateTime value.
-}
toDateTime : ZonedDateTime -> DateTime
toDateTime ((ZonedDateTime { dateTime }) as zonedDateTime) =
    utcOffset zonedDateTime
        |> flip DateTime.addMilliseconds dateTime


{-| fromTimestamp converts the millisecond representation of a UNIX
timestamp into a ZonedDateTime value.  This is equivalent to calling
`DateTime.fromTimestamp` and then converting the resulting `DateTime`
value to a `ZonedDateTime`.
-}
fromTimestamp : TimeZone -> Time -> ZonedDateTime
fromTimestamp timeZone timestamp =
    DateTime.fromTimestamp timestamp
        |> fromDateTime timeZone


{-| toTimestamp converts a ZonedDateTime to its UNIX timestamp
representation in milliseconds.
-}
toTimestamp : ZonedDateTime -> Time
toTimestamp (ZonedDateTime { timeZone, dateTime }) =
    DateTime.toTimestamp dateTime
        |> flip TimeZone.offset timeZone
        |> flip DateTime.addMilliseconds dateTime
        |> DateTime.toTimestamp


{-| timeZone returns a ZonedDatetime's TimeZone.
-}
timeZone : ZonedDateTime -> TimeZone
timeZone (ZonedDateTime { timeZone }) =
    timeZone


{-| asTimeZone converts a ZonedDateTime to another TimeZone.
-}
asTimeZone : TimeZone -> ZonedDateTime -> ZonedDateTime
asTimeZone timeZone =
    fromDateTime timeZone << toDateTime


{-| year returns a ZonedDateTime's year.
-}
year : ZonedDateTime -> Int
year (ZonedDateTime { dateTime }) =
    DateTime.year dateTime


{-| month returns a ZonedDateTime's month.
-}
month : ZonedDateTime -> Int
month (ZonedDateTime { dateTime }) =
    DateTime.month dateTime


{-| day returns a ZonedDateTime's day.
-}
day : ZonedDateTime -> Int
day (ZonedDateTime { dateTime }) =
    DateTime.day dateTime


{-| weekday returns a ZonedDateTime's day of the week.
-}
weekday : ZonedDateTime -> Weekday
weekday (ZonedDateTime { dateTime }) =
    DateTime.weekday dateTime


{-| hour returns a ZonedDateTime's hour.
-}
hour : ZonedDateTime -> Int
hour (ZonedDateTime { dateTime }) =
    DateTime.hour dateTime


{-| minute returns a ZonedDateTime's minute.
-}
minute : ZonedDateTime -> Int
minute (ZonedDateTime { dateTime }) =
    DateTime.minute dateTime


{-| second returns a ZonedDateTime's second.
-}
second : ZonedDateTime -> Int
second (ZonedDateTime { dateTime }) =
    DateTime.second dateTime


{-| millisecond returns a ZonedDateTime's millisecond.
-}
millisecond : ZonedDateTime -> Int
millisecond (ZonedDateTime { dateTime }) =
    DateTime.millisecond dateTime


mapInner : (a -> DateTime -> DateTime) -> a -> ZonedDateTime -> ZonedDateTime
mapInner f x (ZonedDateTime ({ dateTime } as t)) =
    ZonedDateTime { t | dateTime = f x dateTime }


{-| setDate sets a ZonedDateTime's date.
-}
setDate : Date -> ZonedDateTime -> ZonedDateTime
setDate =
    mapInner DateTime.setDate


{-| setYear sets a ZonedDateTime's year.
-}
setYear : Int -> ZonedDateTime -> ZonedDateTime
setYear =
    mapInner DateTime.setYear


{-| setMonth sets a ZonedDateTime's month.
-}
setMonth : Int -> ZonedDateTime -> ZonedDateTime
setMonth =
    mapInner DateTime.setMonth


{-| setDay sets a ZonedDateTime's day.
-}
setDay : Int -> ZonedDateTime -> ZonedDateTime
setDay =
    mapInner DateTime.setDay


{-| setHour sets a ZonedDateTime's hour.
-}
setHour : Int -> ZonedDateTime -> ZonedDateTime
setHour =
    mapInner DateTime.setHour


{-| setMinute sets a ZonedDateTime's minute.
-}
setMinute : Int -> ZonedDateTime -> ZonedDateTime
setMinute =
    mapInner DateTime.setMinute


{-| setSecond sets a ZonedDateTime's second.
-}
setSecond : Int -> ZonedDateTime -> ZonedDateTime
setSecond =
    mapInner DateTime.setSecond


{-| setMillisecond sets a ZonedDateTime's millisecond.
-}
setMillisecond : Int -> ZonedDateTime -> ZonedDateTime
setMillisecond =
    mapInner DateTime.setMillisecond


{-| addYears adds a relative number of years to a ZonedDateTime value.
-}
addYears : Int -> ZonedDateTime -> ZonedDateTime
addYears =
    mapInner DateTime.addYears


{-| addMonths adds a relative number of months to a ZonedDateTime value.
-}
addMonths : Int -> ZonedDateTime -> ZonedDateTime
addMonths =
    mapInner DateTime.addMonths


{-| addDays adds an absolute number of days to a ZonedDateTime value.
-}
addDays : Int -> ZonedDateTime -> ZonedDateTime
addDays =
    mapInner DateTime.addDays


{-| addHours adds a relative number of hours to a ZonedDateTime value.
-}
addHours : Int -> ZonedDateTime -> ZonedDateTime
addHours =
    mapInner DateTime.addHours


{-| addMinutes adds a relative number of minutes to a ZonedDateTime value.
-}
addMinutes : Int -> ZonedDateTime -> ZonedDateTime
addMinutes =
    mapInner DateTime.addMinutes


{-| addSeconds adds a relative number of seconds to a ZonedDateTime value.
-}
addSeconds : Int -> ZonedDateTime -> ZonedDateTime
addSeconds =
    mapInner DateTime.addSeconds


{-| addMilliseconds adds an absolute number of milliseconds to a
ZonedDateTime value.
-}
addMilliseconds : Int -> ZonedDateTime -> ZonedDateTime
addMilliseconds =
    mapInner DateTime.addMilliseconds


{-| abbreviation returns a ZonedDateTime's abbreviation at that time.
-}
abbreviation : ZonedDateTime -> String
abbreviation ((ZonedDateTime { timeZone }) as zonedDateTime) =
    toTimestamp zonedDateTime
        |> flip TimeZone.abbreviation timeZone


{-| utcOffset returns a ZonedDateTime's offset from UTC in
milliseconds at that time.
-}
utcOffset : ZonedDateTime -> Int
utcOffset ((ZonedDateTime { timeZone }) as zonedDateTime) =
    toTimestamp zonedDateTime
        |> flip TimeZone.offset timeZone


{-| utcOffsetString returns a ZonedDateTime's UTC offset at that time
as a string.
-}
utcOffsetString : ZonedDateTime -> String
utcOffsetString ((ZonedDateTime { timeZone }) as zonedDateTime) =
    toTimestamp zonedDateTime
        |> flip TimeZone.offsetString timeZone


{-| toISO8601 renders a ZonedDateTime in ISO8601 format.
-}
toISO8601 : ZonedDateTime -> String
toISO8601 dateTime =
    toString (year dateTime)
        ++ "-"
        ++ padded (month dateTime)
        ++ "-"
        ++ padded (day dateTime)
        ++ "T"
        ++ padded (hour dateTime)
        ++ ":"
        ++ padded (minute dateTime)
        ++ ":"
        ++ padded (second dateTime)
        ++ "."
        ++ padded3 (millisecond dateTime)
        ++ utcOffsetString dateTime


{-| fromISO8601 parses an ISO8601-formatted string into a
ZonedDateTime object, adjusting for its offset.
-}
fromISO8601 : TimeZone -> String -> Result String ZonedDateTime
fromISO8601 timeZone input =
    DateTime.fromISO8601 input
        |> Result.map (fromDateTime timeZone)
