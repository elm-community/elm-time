# elm-time [![Build Status](https://travis-ci.org/elm-community/elm-time.svg)](https://travis-ci.org/elm-community/elm-time)

``` shell
elm package install elm-community/elm-time
```

## This Package is Released to Elm 0.19 -- Also Deprecated!

Isaac Seymour has ported **elm-community/elm-time** to
**[isaacseymour/deprecated-time](https://package.elm-lang.org/packages/isaacseymour/deprecated-time/latest)**.
With the change in **elm/time** to use Posix, and a consensus to break this package up so that you don't have to
"include everything" to use it, we decided that this was a good time to deprecate.

That being said, we will continue to support `isaacseymour/deprecated-time` with bug fixes for the foreseeable future.

Thanks to Isaac for performing this upgrade!

## Major Changes!

This release prepares **elm-time** to be upgraded to Elm `0.19` by changing
out the **ISO8601** and **Timezone Name** parsing from the 
**[parser-combinators](http://package.elm-lang.org/packages/elm-community/parser-combinators/latest)** parser to
Evan's **[parser](http://package.elm-lang.org/packages/elm-tools/parser/latest)**.

> NOTE: this release is probably the last Elm `0.18` release.

Hence, the changes are extensive and some API's have changed.  Here's a summary
of them:

* **ISO8601** processing has been broken out into its own module: `Time.Iso8601`.
* An "Elm-style" error renderer for **ISO8601** parsing errors is provided: `Time.Iso8601ErrorMsg`.
* An example Elm client-application showing the error handling is provided in `/examples/with-parser-error-renderer`.
* Each of the public APIs in `Time.Date`, `Time.DateTime`, `Time.ZonedDateTime`, `Iso8601`, and `Iso8601ErrorMsg`
now has extensive
**["verify examples"](https://github.com/stoeffel/elm-verify-examples)** documentation.

## Examples

* **[Error Rendering Example](https://github.com/elm-community/elm-time/blob/daa0e1b60a912519af5b699cc26c0b17a6e06257/examples/with-parser-error-renderer/README.md)**
* **[Running and Understanding Timezone Examples](https://github.com/elm-community/elm-time/wiki/The-Examples)**

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
> d = date 1992 5 29
Date { year = 1992, month = 5, day = 29 } : Date

> Date.year d
1992 : Int

> Date.month d
5 : Int

> Date.day d
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
import Time.Date as Date exposing (Date, date, addYears)
import Time.Iso8601

> date 1992 1 31
|   |> addYears 1
|   |> Time.Iso8601.fromDate
"1993-01-31" : String

> date 1992 2 29
|   |> addYears 1
|   |> Time.Iso8601.fromDate
"1993-02-28" : String

> date 1992 1 31
|   |> Date.addMonths 1
|   |> Time.Iso8601.fromDate
"1992-02-28" : String
```

## DateTimes

`DateTimes` represent a `Date` together with the time information starting on midnight for the `Date`.

``` elm
import Time.DateTime as DateTime exposing (DateTime, dateTime, year, month, day, hour, minute, second, millisecond)
```

### Constructing DateTimes

`DateTime`s can be constructed from a record using the `dateTime`
function or from a UTC timestamp in milliseconds using `fromTimestamp`.
To construct a `DateTime` using `dateTime`, pass it a record
containing fields for `year`, `month`, `day`, `hour`, `minute`,
`second` and `millisecond`:

``` elm
dt : DateTime
dt =
    dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 }
    
year dt --> 1992
month dt --> 5
day dt --> 29
hour dt --> 0
minute dt --> 0
second --> 0
millisecond --> 0    

dt : DateTime
dt =
    dateTime { year = 1992, month = 2, day = 31, hour = 0, minute = 0, second = 0, millisecond = 0 }
    
year dt --> 1992
month dt --> 2
day dt --> 29 - Note clamped.
hour dt --> 0
minute dt --> 0
second --> 0
millisecond --> 0    

dt : DateTime
dt =
    dateTime { year = 1993, month = 2, day = 31, hour = 0, minute = 0, second = 0, millisecond = 0 }
    
year dt --> 1993
month dt --> 2
day dt --> 28 - Note clamped.
hour dt --> 0
minute dt --> 0
second --> 0
millisecond --> 0    
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
