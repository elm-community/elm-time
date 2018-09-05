module Time.DateTime exposing
    ( DateTime
    , zero, epoch, dateTime, makeDateTime, fromPosix, toPosix
    , date, year, month, day, weekday, hour, minute, second, millisecond
    , setDate, setYear, setMonth, setDay, setHour, setMinute, setSecond, setMillisecond, addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMilliseconds
    , compare, DateTimeDelta, delta
    , isValidTime
    )

{-| This module defines a time representation based on a Date and the
time of day.


# DateTimes

@docs DateTime


# Constructing DateTimes

@docs zero, epoch, dateTime, makeDateTime, fromPosix, toPosix


# Inspecting DateTimes

@docs date, year, month, day, weekday, hour, minute, second, millisecond


# Manipulating DateTimes

@docs setDate, setYear, setMonth, setDay, setHour, setMinute, setSecond, setMillisecond, addYears, addMonths, addDays, addHours, addMinutes, addSeconds, addMilliseconds


# Comparing DateTimes

@docs compare, DateTimeDelta, delta


# Helper functions

@docs isValidTime

-}

import Time exposing (Posix)
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


{-| DateTimeDelta represents the difference between two
DateTime values in terms of each of the different "units".

See `Time.DateTime.delta` for an "aha!" example.

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

    import Time.Iso8601

    dateTime zero
    |> Time.Iso8601.fromDateTime
    --> "0-01-01T00:00:00.000Z"

    dateTime { zero | year = 2016 }
    |> Time.Iso8601.fromDateTime
    --> "2016-01-01T00:00:00.000Z"

    dateTime { zero | year = 2016, month = 5, day = 29, hour = 13 }
    |> Time.Iso8601.fromDateTime
    --> "2016-05-29T13:00:00.000Z"

-}
zero : DateTimeData
zero =
    Time.Internal.zero


{-| epoch is the instant in time that represents the first millisecond
of the UNIX Epoch.

    import Time.Iso8601

    epoch
    |> Time.Iso8601.fromDateTime
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
dateTime data =
    DateTime
        { date = Time.Date.date data.year data.month data.day
        , offset = offsetFromTimeData data
        }


mkDateTime : Date -> TimeData d -> DateTime
mkDateTime date_ time =
    DateTime { date = date_, offset = offsetFromTimeData time }


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
makeDateTime date_ offset =
    DateTime { date = date_, offset = offset }


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
date (DateTime data) =
    data.date


{-| year returns a DateTime's year.

    dateTime { zero | year = 2015 }
    |> year
    --> 2015

-}
year : DateTime -> Int
year (DateTime data) =
    Time.Date.year data.date


{-| month returns a DateTime's month.

    dateTime { zero | month = 7 }
    |> month
    --> 7

    dateTime { zero | month = 0 } -- will be clamped
    |> month
    --> 1

-}
month : DateTime -> Int
month (DateTime data) =
    Time.Date.month data.date


{-| day returns a DateTime's day.

    dateTime { zero | day = 31 }
    |> day
    --> 31

    dateTime { zero | day = 32 } -- will be clamped
    |> day
    --> 31

-}
day : DateTime -> Int
day (DateTime data) =
    Time.Date.day data.date


{-| weekday returns a DateTime's day of the week.

    import Time.Date

    dateTime { zero | year = 2018, month = 5, day = 27 }
    |> weekday
    --> Time.Date.Sun

-}
weekday : DateTime -> Weekday
weekday (DateTime data) =
    Time.Date.weekday data.date


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
    modBy hourMs offset // minuteMs


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
    modBy minuteMs (modBy hourMs offset) // secondMs


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
    modBy secondMs (modBy minuteMs (modBy hourMs offset))


{-| setDate sets a DateTime's Date.

    import Time.Date as TD

    dateTime zero
    |> setDate (TD.date 2018 5 27)
    |> date
    --> TD.date 2018 5 27

-}
setDate : Date -> DateTime -> DateTime
setDate newDate (DateTime { offset }) =
    DateTime
        { date = newDate
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
setYear newYear (DateTime data) =
    DateTime
        { date = Time.Date.setYear newYear data.date
        , offset = data.offset
        }


{-| setMonth sets a DateTime's month.

See also `Time.Date.setMonth`.

    dateTime zero
    |> setMonth 12
    |> month
    --> 12

-}
setMonth : Int -> DateTime -> DateTime
setMonth newMonth (DateTime data) =
    DateTime
        { date = Time.Date.setMonth newMonth data.date
        , offset = data.offset
        }


{-| setDay sets a DateTime's day.

See also `Time.Date.setDay`.

    dateTime zero
    |> setDay 31
    |> day
    --> 31

-}
setDay : Int -> DateTime -> DateTime
setDay newDay (DateTime data) =
    DateTime
        { date = Time.Date.setDay newDay data.date
        , offset = data.offset
        }


{-| setHour sets a DateTime's hour.

    dateTime zero
    |> setHour 23
    |> hour
    --> 23

-}
setHour : Int -> DateTime -> DateTime
setHour newHour ((DateTime data) as t) =
    mkDateTime data.date
        { hour = newHour
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
setMinute newMinute ((DateTime data) as t) =
    mkDateTime data.date
        { hour = hour t
        , minute = newMinute
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
setSecond newSecond ((DateTime data) as t) =
    mkDateTime data.date
        { hour = hour t
        , minute = minute t
        , second = newSecond
        , millisecond = millisecond t
        }


{-| setMillisecond sets a DateTime's millisecond.

    dateTime zero
    |> setMillisecond 999
    |> millisecond
    --> 999

-}
setMillisecond : Int -> DateTime -> DateTime
setMillisecond newMillisecond ((DateTime data) as t) =
    mkDateTime data.date
        { hour = hour t
        , minute = minute t
        , second = second t
        , millisecond = newMillisecond
        }


{-| addYears adds a relative number of years to a DateTime value.

See also `Time.Date.addYears`.

    dateTime { zero | year = 2016 }
    |> addYears 2
    |> year
    --> 2018

-}
addYears : Int -> DateTime -> DateTime
addYears years (DateTime data) =
    DateTime
        { date = Time.Date.addYears years data.date
        , offset = data.offset
        }


{-| addMonths adds a relative number of months to a DateTime value.

See also `Time.Date.addMonths`.

    dateTime { zero | month = 1 }
    |> addMonths 1
    |> month
    --> 2

-}
addMonths : Int -> DateTime -> DateTime
addMonths months (DateTime data) =
    DateTime
        { date = Time.Date.addMonths months data.date
        , offset = data.offset
        }


{-| addDays adds a relative number of days to a DateTime value.

See also `Time.Date.addDays`.

    dateTime { zero | day = 20 }
    |> addDays -11
    |> day
    --> 9

-}
addDays : Int -> DateTime -> DateTime
addDays days (DateTime data) =
    DateTime
        { date = Time.Date.addDays days data.date
        , offset = data.offset
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
addMilliseconds ms (DateTime data) =
    let
        total =
            ms + data.offset

        ( days, newOffset ) =
            if total < 0 then
                let
                    days_ =
                        -(abs total // dayMs + 1)

                    offset =
                        remainderBy dayMs total
                in
                if offset == 0 then
                    ( days_ + 1, 0 )

                else
                    ( days_, dayMs + remainderBy dayMs offset )

            else
                ( total // dayMs, remainderBy dayMs total )
    in
    DateTime
        { date = Time.Date.addDays days data.date
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
    let
        -- Pad each part to 3 characters, join them and compare like that
        stringify dt =
            [ year, month, day, hour, minute, second, millisecond ]
                |> List.map (\fn -> fn dt)
                |> List.map (String.fromInt >> String.padLeft 3 ' ')
                |> String.join ""
    in
    Basics.compare (stringify dt1) (stringify dt2)


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

    -- Note what is counted is the number of transitions
    -- to get from one unit to another.  Hence
    -- the following shows that the difference of
    -- 1 day makes a big difference when it separates
    -- Dec 31 and Jan 1 as shown here.  Observe
    -- that years, months, and days are the same because
    -- each only requires one transition to get from
    -- one date to the other.

    upper
    |> addDays -1
    |> delta upper
    --> { years = 1
    --> , months = 1
    --> , days = 1
    --> , hours = 24
    --> , minutes = 1440
    --> , seconds = 86400
    --> , milliseconds = 86400000
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
isValidTime h m s ms =
    h >= 0 && h < 24 && m >= 0 && m < 60 && s >= 0 && s < 60 && ms >= 0 && ms < 1000


{-| toPosix converts a DateTime value to the equivalent elm/time Posix
representation.

    import Time

    epoch
    |> toPosix
    --> Time.millisToPosix 0

-}
toPosix : DateTime -> Posix
toPosix time =
    delta time epoch
        |> .milliseconds
        |> Time.millisToPosix


{-| fromPosix converts the elm/time Posix representation of a
UNIX timestamp into a DateTime value.

    import Time
    0 |> Time.millisToPosix |> fromPosix
    --> epoch

-}
fromPosix : Posix -> DateTime
fromPosix posix =
    addMilliseconds (Time.posixToMillis posix) epoch
