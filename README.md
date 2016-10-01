# elm-time [![Build Status](https://travis-ci.org/Bogdanp/elm-time.svg)](https://travis-ci.org/Bogdanp/elm-time)

``` shell
elm package install -y Bogdanp/elm-time
```

# Usage

## Dates

`Date`s may represent any date in the [proleptic Gregorian calendar][cal].

``` elm
import Time.Date as Date exposing (Date, date)
```

### Constructing Dates

Use `date` to construct `Date` values.  If given a valid date, the
function returns `Just` that date, otherwise it returns `Nothing`.

``` elm
> date 1992 2 28
Just (Date { year = 1992, month = 2, day = 28 }) : Maybe Date
> date 1992 2 31
Nothing : Maybe Date
```

``` elm
> let
|   birthday : Maybe Date
|   birthday = date 1992 5 29
| in
|   Maybe.map Date.toString birthday
Just "1992-05-29"
```

Use `year`, `month`, and `day` to inspect `Date`s.

``` elm
> Maybe.map Date.year birthday
Just 1992 : Maybe Int
> Maybe.map Date.month birthday
Just 5 : Maybe Int
> Maybe.map Date.day birthday
Just 29 : Maybe Int
```

### Manipulating Dates

`FIXME`

### Comparing Dates

`FIXME`

## DateTimes

`DateTimes` represent a `Date` together with a time offset from midnight.

``` elm
import Time.DateTime as DateTime exposing (DateTime)
```

### Constructing DateTimes

`FIXME`

### Manipulating DateTimes

`FIXME`

### Comparing DateTimes

`FIXME`

### Helpers

`FIXME`

## ZonedDateTimes

`ZonedDateTimes` represent a `DateTime` in a specific `TimeZone`.

``` elm
ipmort Time.TimeZones as TimeZones
import Time.ZonedDateTime as ZonedDateTime exposing (ZonedDateTime)
```


[cal]: https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar
