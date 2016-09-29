module UTC.ZonedDateTime
    exposing
        ( ZonedDateTime
        , fromDateTime
        , toDateTime
        , timeZone
        , year
        , month
        , day
        , hour
        , minute
        , second
        , millisecond
        , abbreviation
        , utcOffset
        , utcOffsetString
        , toISO8601
        )

{-| This module defines a time representation based on a Date, the
time of day and a time zone.

ZonedDateTimes should only be used when reasoning about or displaying
`DateTime`s in a user's local time zone.  For this reason, the API
surface of `ZonedDateTimes` is extremely limited.

# ZonedDateTimes
@docs ZonedDateTime

# Constructing ZonedDateTimes
@docs fromDateTime, toDateTime

# Inspecting ZonedDateTimes
@docs timeZone, year, month, day, hour, minute, second, millisecond, abbreviation, utcOffset, utcOffsetString, toISO8601
-}

import UTC.DateTime as DateTime exposing (DateTime)
import UTC.Internal exposing (..)
import UTC.TimeZone as TimeZone exposing (TimeZone)


{-| ZoneDateTime is the opaque type for all ZonedDateTime values.
Values of this type represent a `(TimeZone, DateTime)` pair.
-}
type ZonedDateTime
    = ZonedDateTime
        { timeZone : TimeZone
        , dateTime : DateTime
        }


{-| fromDateTime constructs a ZonedDateTime value from a TimeZone and a DateTime.
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
            , dateTime = DateTime.addMinutes (round -offset) dateTime
            }


{-| toDateTime converts a ZonedDateTime to a UTC DateTime value.
-}
toDateTime : ZonedDateTime -> DateTime
toDateTime (ZonedDateTime { timeZone, dateTime }) =
    let
        timestamp =
            DateTime.toTimestamp dateTime

        offset =
            TimeZone.offset timestamp timeZone
    in
        DateTime.addMinutes (round offset) dateTime


{-| timeZone returns a ZonedDatetime's TimeZone.
-}
timeZone : ZonedDateTime -> TimeZone
timeZone (ZonedDateTime { timeZone }) =
    timeZone


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


{-| abbreviation returns a ZonedDateTime's abbreviation at that time.
-}
abbreviation : ZonedDateTime -> String
abbreviation (ZonedDateTime { timeZone, dateTime }) =
    DateTime.toTimestamp dateTime
        |> flip TimeZone.abbreviation timeZone


{-| utcOffset returns a ZonedDateTime's offset from UTC in minutes at
that time.
-}
utcOffset : ZonedDateTime -> Float
utcOffset (ZonedDateTime { timeZone, dateTime }) =
    DateTime.toTimestamp dateTime
        |> flip TimeZone.offset timeZone


{-| utcOffsetString returns a ZonedDateTime's offsetString from UTC in minutes at
that time.
-}
utcOffsetString : ZonedDateTime -> String
utcOffsetString (ZonedDateTime { timeZone, dateTime }) =
    DateTime.toTimestamp dateTime
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
        ++ utcOffsetString dateTime
