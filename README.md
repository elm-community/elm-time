# elm-time [![Build Status](https://travis-ci.org/elm-community/elm-time.svg)](https://travis-ci.org/elm-community/elm-time)

``` shell
elm package install elm-community/elm-time
```

* **[Running and Understanding Examples](https://github.com/elm-community/elm-time/wiki/The-Examples)**

## Dates

`Date`s may represent any date in the [proleptic Gregorian calendar][cal].

``` elm
import Time.Date as Date exposing (Date, date)
```

### Constructing Dates

Use `date` to construct `Date` values.  If given invalid values for
the month and day, they are both clamped and the nearest valid date is
returned.

``` elm
> date 1992 2 28
Date { year = 1992, month = 2, day = 28 } : Date

> date 1992 2 31
Date { year = 1992, month = 2, day = 29 } : Date

> date 1992 2 128
Date { year = 1992, month = 2, day = 29 } : Date
```

Use `year`, `month`, and `day` to inspect `Date`s.

``` elm
> birthday = date 1992 5 29
Date { year = 1992, month = 5, day = 29 } : Date

> Date.year birthday
1992 : Int

> Date.month birthday
5 : Int

> Date.day birthday
29 : Int
```

### Manipulating Dates

`setYear`, `setMonth` and `setDay` can be used to create new `Dates`
containing updated values for each respective field.  Like `date`,
these functions clamp their parameters and return the nearest valid
date.

`addDays` can be used to add an exact number of days to a `Date`.

`addYears` and `addMonths` add a relative number of years and months
to a date.  If the target date is invalid, these functions continually
subtract one day until a valid date is found.

``` elm
> date 1992 1 31
|   |> Date.addYears 1
|   |> Date.toISO8601
"1993-01-31" : String

> date 1992 2 29
|   |> Date.addYears 1
|   |> Date.toISO8601
"1993-02-28" : String

> date 1992 1 31
|   |> Date.addMonths 1
|   |> Date.toISO8601
"1992-02-28" : String
```

## DateTimes

`DateTimes` represent a `Date` together with a time offset from midnight.

``` elm
import Time.DateTime as DateTime exposing (DateTime, dateTime)
```

### Constructing DateTimes

`DateTime`s can be constructed from a record using the `dateTime`
function or from a UTC timestamp in milliseconds using `fromTimestamp`.
To construct a `DateTime` using `dateTime`, pass it a record
containing fields for `year`, `month`, `day`, `hour`, `minute`,
`second` and `millisecond`:

``` elm
> dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 }
DateTime { date = Date { year = 1992, month = 5, day = 29 }, offset = 0 } : Date

> dateTime { year = 1992, month = 2, day = 31, hour = 0, minute = 0, second = 0, millisecond = 0 }
DateTime { date = Date { year = 1992, month = 2, day = 29 }, offset = 0 } : Date
```

To make constructing `DateTimes` less tedious, the library provides
`Time.DateTime.zero`:

``` elm
> import Time.DateTime as DateTime exposing (DateTime, dateTime, zero)

> dateTime { zero | year = 1992 }
|   |> DateTime.toISO8601
"1992-01-01T00:00:00.000Z" : String

> dateTime { zero | year = 1992, month = 2, day = 28, hour = 5 }
|   |> DateTime.toISO8601
"1992-02-28T05:00:00.000Z" : String
```

Use `fromTimestamp` to construct a `DateTime` from a UTC timestamp in
milliseconds:

``` elm
> fromTimestamp 0
|   |> DateTime.toISO8601
"1970-01-01T00:00:00.000Z" : String
```

See `examples/without-timezone` for an example of how to construct
`DateTime`s from local time.

### Manipulating DateTimes

Like `Time.Date`, the `DateTime` module exposes functions for adding
to and updating a `DateTime`'s fields.  The functions `addYears` and
`addMonths` have the same behaviour as their `Time.Date` counterparts.

## ZonedDateTimes

`ZonedDateTimes` represent a `DateTime` in a specific `TimeZone`. See
`examples/with-timezone` for an example of how to use `ZonedDateTime`s.

``` elm
import Time.TimeZones as TimeZones
import Time.ZonedDateTime as ZonedDateTime exposing (ZonedDateTime)
```


[cal]: https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar
