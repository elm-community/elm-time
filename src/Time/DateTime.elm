module Time.DateTime
    exposing
        ( DateTime
        , DateTimeDelta
        , dateTime
        , zero
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
        , compare
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

# DateTimes
@docs DateTime, zero, epoch, dateTime, date, year, month, day, hour, minute, second, millisecond

# Manipulating DateTimes
@docs setDate, setYear, setMonth, setDay, setHour, setMinute, setSecond, setMillisecond, addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMilliseconds

# Comparing DateTimes
@docs compare

# Subtracting DateTimes
@docs DateTimeDelta, delta

# Helper functions
@docs toTimestamp, fromTimestamp, toTuple, fromTuple, toISO8601, fromISO8601
-}

import Combine
import Combine.Infix exposing (..)
import Combine.Num
import String
import Time exposing (Time)
import Time.Date exposing (Date)
import Time.Internal exposing (..)


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


{-| zero represents the first millisecond of the first day of the
current era.  Use it to construct `DateTime` values:

    -- 0-01-01T00:00:00Z
    dateTime zero

    -- 2016-01-01T00:00:00Z
    dateTime { zero | year = 2016 }

    -- 2016-05-29T13:00:00Z
    dateTime { zero | year = 2016, month = 5, day = 29, hour = 13 }
-}
zero : DateTimeData
zero =
    Time.Internal.zero


{-| epoch is the instant in time that represents the first millisecond
of the UNIX Epoch.
-}
epoch : DateTime
epoch =
    case dateTime { zero | year = 1970 } of
        Nothing ->
            Debug.crash "epoch: failed to construct epoch date"

        Just d ->
            d


{-| dateTime constructs a DateTime value given a date and a time.  If
the constructed value is invalid, Nothing is returned.
-}
dateTime : DateTimeData -> Maybe DateTime
dateTime ({ year, month, day } as data) =
    Maybe.map2
        (\date offset -> DateTime { date = date, offset = offset })
        (Time.Date.date year month day)
        (offsetFromTimeData data)


dateTime' : Date -> TimeData d -> Maybe DateTime
dateTime' date time =
    offsetFromTimeData time
        |> Maybe.map (\offset -> DateTime { date = date, offset = offset })


{-| date returns a DateTime's Date.
-}
date : DateTime -> Date
date (DateTime { date }) =
    date


{-| year returns a DateTime's year.
-}
year : DateTime -> Int
year (DateTime { date }) =
    Time.Date.year date


{-| month returns a DateTime's month.
-}
month : DateTime -> Int
month (DateTime { date }) =
    Time.Date.month date


{-| day returns a DateTime's day.
-}
day : DateTime -> Int
day (DateTime { date }) =
    Time.Date.day date


{-| hour returns a DateTime's hour.
-}
hour : DateTime -> Int
hour (DateTime { offset }) =
    offset // hourMs


{-| minute returns a DateTime's minute.
-}
minute : DateTime -> Int
minute (DateTime { offset }) =
    (offset % hourMs) // minuteMs


{-| second returns a DateTime's second.
-}
second : DateTime -> Int
second (DateTime { offset }) =
    (offset % hourMs % minuteMs) // secondMs


{-| millisecond returns a DateTime's millisecond.
-}
millisecond : DateTime -> Int
millisecond (DateTime { offset }) =
    offset % hourMs % minuteMs % secondMs


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

See also `Time.Date.setYear`.
-}
setYear : Int -> DateTime -> Maybe DateTime
setYear year (DateTime { date, offset }) =
    Time.Date.setYear year date
        |> Maybe.map (\d -> DateTime { date = d, offset = offset })


{-| setMonth sets a DateTime's month, returning Nothing if the updated
time is invalid or Just the new DateTime.

See also `Time.Date.setMonth`.
-}
setMonth : Int -> DateTime -> Maybe DateTime
setMonth month (DateTime { date, offset }) =
    Time.Date.setMonth month date
        |> Maybe.map (\d -> DateTime { date = d, offset = offset })


{-| setDay sets a DateTime's day, returning Nothing if the updated
time is invalid or Just the new DateTime.

See also `Time.Date.setDay`.
-}
setDay : Int -> DateTime -> Maybe DateTime
setDay day (DateTime { date, offset }) =
    Time.Date.setDay day date
        |> Maybe.map (\d -> DateTime { date = d, offset = offset })


{-| setHour sets a DateTime's hour, returning Nothing if the updated
time is invalid or Just the new DateTime.
-}
setHour : Int -> DateTime -> Maybe DateTime
setHour hour ((DateTime { date }) as t) =
    dateTime' date
        { hour = hour
        , minute = minute t
        , second = second t
        , millisecond = millisecond t
        }


{-| setMinute sets a DateTime's minute, returning Nothing if the
updated time is invalid or Just the new DateTime.
-}
setMinute : Int -> DateTime -> Maybe DateTime
setMinute minute ((DateTime { date }) as t) =
    dateTime' date
        { hour = hour t
        , minute = minute
        , second = second t
        , millisecond = millisecond t
        }


{-| setSecond sets a DateTime's second, returning Nothing if the
updated time is invalid or Just the new DateTime.
-}
setSecond : Int -> DateTime -> Maybe DateTime
setSecond second ((DateTime { date }) as t) =
    dateTime' date
        { hour = hour t
        , minute = minute t
        , second = second
        , millisecond = millisecond t
        }


{-| setMillisecond sets a DateTime's millisecond, returning Nothing if
the updated time is invalid or Just the new DateTime.
-}
setMillisecond : Int -> DateTime -> Maybe DateTime
setMillisecond millisecond ((DateTime { date }) as t) =
    dateTime' date
        { hour = hour t
        , minute = minute t
        , second = second t
        , millisecond = millisecond
        }


{-| addYears adds a relative number of years to a DateTime value.

See also `Time.Date.addYears`.
-}
addYears : Int -> DateTime -> DateTime
addYears years (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.addYears years date
        , offset = offset
        }


{-| addMonths adds a relative number of months to a DateTime value.

See also `Time.Date.addMonths`.
-}
addMonths : Int -> DateTime -> DateTime
addMonths months (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.addMonths months date
        , offset = offset
        }


{-| addDays adds an absolute number of days to a DateTime value.

See also `Time.Date.addDays`.
-}
addDays : Int -> DateTime -> DateTime
addDays days (DateTime { date, offset }) =
    DateTime
        { date = Time.Date.addDays days date
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
            , offset = offset'
            }


{-| compare two DateTimes.
-}
compare : DateTime -> DateTime -> Order
compare dt1 dt2 =
    -- comparison of 7-tuples is not supported so we use toISO8601 instead
    Basics.compare (toISO8601 dt1) (toISO8601 dt2)


{-| delta computes the relative difference between two DateTime values.
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
        ( year, month, day ) =
            Time.Date.toTuple date
    in
        ( year, month, day, hour t, minute t, second t, millisecond t )


{-| fromTuple converts a (year, month, day, hour, minute, second,
millisecond) tuple into a DateTime.  If the tuple represents an invalid
DateTime then Nothing is returned.
-}
fromTuple : ( Int, Int, Int, Int, Int, Int, Int ) -> Maybe DateTime
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


{-| fromISO8601 parses an ISO8601-formatted date time string into a
DateTime object, adjusting for its offset.
-}
fromISO8601 : String -> Result String DateTime
fromISO8601 input =
    let
        paddedInt =
            Combine.optional "" (Combine.string "0") *> Combine.Num.int

        intRange lo hi =
            paddedInt
                `Combine.andThen`
                    (\n ->
                        if n >= lo && n <= hi then
                            Combine.succeed n
                        else
                            Combine.fail [ "expected an integer in the range [" ++ toString lo ++ ", " ++ toString hi ++ "]" ]
                    )

        date =
            (,,)
                <$> Combine.Num.int
                <*> (Combine.string "-" *> intRange 1 12)
                <*> (Combine.string "-" *> intRange 1 31)

        time =
            (,,)
                <$> (Combine.string "T" *> intRange 0 23)
                <*> (Combine.string ":" *> intRange 0 59)
                <*> (Combine.string ":" *> intRange 0 59)

        minutes =
            (\s h m -> s * h * 60 + s * m)
                <$> Combine.Num.sign
                <*> intRange 0 23
                <*> (Combine.string ":" *> intRange 0 59)

        offset =
            (0 <$ Combine.string "Z") <|> minutes

        datetime =
            (,,)
                <$> date
                <*> time
                <*> offset
                <* Combine.end

        convert ( ( year, month, day ), ( hour, minute, second ), offset ) =
            case dateTime (DateTimeData year month day hour minute second 0) of
                Nothing ->
                    Combine.fail [ "invalid date" ]

                Just datetime ->
                    Combine.succeed <| addMinutes -offset datetime
    in
        case Combine.parse (datetime `Combine.andThen` convert) input of
            ( Err es, { position } ) ->
                let
                    messages =
                        String.join " or " es
                in
                    Err ("Errors encountered at position " ++ toString position ++ ": " ++ messages)

            ( Ok dt, _ ) ->
                Ok dt
