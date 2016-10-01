module UTC.ZonedDateTime
    exposing
        ( ZonedDateTime
        , zero
        , zonedDateTime
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
@docs zero, zonedDateTime, fromDateTime, toDateTime

# Inspecting ZonedDateTimes
@docs timeZone, year, month, day, hour, minute, second, millisecond, abbreviation, utcOffset, utcOffsetString

# Helper functions
@docs toISO8601, fromISO8601
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
    UTC.Internal.zero


{-| zonedDateTime constructs a ZonedDateTime value given a TimeZone, a
date and a time.  If the constructed value is invalid, Nothing is
returned.
-}
zonedDateTime : TimeZone -> DateTimeData -> Maybe ZonedDateTime
zonedDateTime timeZone dateTimeData =
    DateTime.dateTime dateTimeData
        |> Maybe.map
            (\dateTime ->
                ZonedDateTime
                    { timeZone = timeZone
                    , dateTime = dateTime
                    }
            )


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
toDateTime (ZonedDateTime { timeZone, dateTime }) =
    let
        timestamp =
            DateTime.toTimestamp dateTime

        offset =
            TimeZone.offset timestamp timeZone
    in
        DateTime.addMilliseconds offset dateTime


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


{-| utcOffset returns a ZonedDateTime's offset from UTC in
milliseconds at that time.
-}
utcOffset : ZonedDateTime -> Int
utcOffset (ZonedDateTime { timeZone, dateTime }) =
    DateTime.toTimestamp dateTime
        |> flip TimeZone.offset timeZone


{-| utcOffsetString returns a ZonedDateTime's UTC offset at that time
as a string.
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


{-| fromISO8601 parses an ISO8601-formatted string into a
ZonedDateTime object, adjusting for its offset.
-}
fromISO8601 : TimeZone -> String -> Result String ZonedDateTime
fromISO8601 timeZone input =
    DateTime.fromISO8601 input
        |> Result.map (fromDateTime timeZone)
