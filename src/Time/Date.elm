module Time.Date exposing
    ( Date
    , date, fromTuple, toTuple
    , year, month, day, Weekday(..), weekday
    , setYear, setMonth, setDay, addYears, addMonths, addDays
    , compare, DateDelta, delta
    , isValidDate, isLeapYear, daysInMonth
    )

{-| This module defines a timezone-independent Date type which can
represent any date of the proleptic Gregorian calendar.


# Dates

@docs Date


# Constructing Dates

@docs date, fromTuple, toTuple


# Inspecting Dates

@docs year, month, day, Weekday, weekday


# Manipulating Dates

@docs setYear, setMonth, setDay, addYears, addMonths, addDays


# Comparing Dates

@docs compare, DateDelta, delta


# Helper functions

@docs isValidDate, isLeapYear, daysInMonth

-}


{-| Date is the opaque type for all Date values. Values of this type
are guaranteed to represent valid proleptic Gregorian calendar dates.
-}
type Date
    = Date
        { year : Int
        , month : Month
        , day : Int
        }


{-| Data type used to represent the days of the week.
-}
type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


{-| Data type used to represent months.
-}
type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


{-| DateDelta represents a delta between two dates.
-}
type alias DateDelta =
    { years : Int
    , months : Int
    , days : Int
    }


{-| date constructs a Date value given a year, a month and a day.
Invalid values are clamped to the nearest valid date.

    d : Date
    d =
        date 2018 5 29

    year d --> 2018
    month d --> 5
    day d --> 29

-}
date : Int -> Int -> Int -> Date
date year_ month_ day_ =
    firstValid year_ (clampMonth month_) (clampDay day_)


{-| year returns a Date's year as an Int.

    year (date 2018 5 26)
    --> 2018

-}
year : Date -> Int
year (Date inner) =
    inner.year


{-| month returns a Date's month as an Int. Guaranteed to be in the
range [1, 12].

    month (date 2018 13 26) -- Note month will be clamped
    --> 12

-}
month : Date -> Int
month (Date inner) =
    monthToInt inner.month


{-| day returns a Date's year as an Int. Guaranteed to be valid for
the Date's (year, month) pair and in the range [1, 31].

    day (date 2018 2 28)
    --> 28

    day (date 2018 2 29)
    --> 28    -- observed clamped

    day (date 2000 2 29)
    --> 29    -- leap year

-}
day : Date -> Int
day (Date inner) =
    inner.day


{-| weekday returns the day of week for a given Date.

This uses Sakamoto's method to determine the day of week.

    weekday (date 2018 5 26)
    --> Sat

-}
weekday : Date -> Weekday
weekday (Date inner) =
    let
        m =
            case inner.month of
                Jan ->
                    0

                Feb ->
                    3

                Mar ->
                    2

                Apr ->
                    5

                May ->
                    0

                Jun ->
                    3

                Jul ->
                    5

                Aug ->
                    1

                Sep ->
                    4

                Oct ->
                    6

                Nov ->
                    2

                Dec ->
                    4

        y =
            case inner.month of
                Jan ->
                    inner.year - 1

                Feb ->
                    inner.year - 1

                _ ->
                    inner.year

        d =
            modBy 7 (y + y // 4 - y // 100 + y // 400 + m + inner.day)
    in
    if d == 0 then
        Sun

    else if d == 1 then
        Mon

    else if d == 2 then
        Tue

    else if d == 3 then
        Wed

    else if d == 4 then
        Thu

    else if d == 5 then
        Fri

    else
        Sat


{-| setYear updates a Date's year. Invalid values are clamped to the
nearest valid date.

    date 2000 5 26
    |> setYear 2016
    |> year
    --> 2016

-}
setYear : Int -> Date -> Date
setYear newYear (Date d) =
    firstValid newYear d.month d.day


{-| setMonth updates a Date's month. Invalid values are clamped to the
nearest valid date.

    date 2016 5 26
    |> setMonth 6
    |> month
    --> 6

    date 2016 5 26
    |> setMonth 13 -- will be clamped
    |> month
    --> 12

-}
setMonth : Int -> Date -> Date
setMonth newMonth (Date d) =
    firstValid d.year (clampMonth newMonth) d.day


{-| setDay updates a Date's day. Invalid values are clamped to the
nearest valid date.

    date 2016 2 26
    |> setDay 28
    |> day
    --> 28

    date 2016 2 28
    |> setDay 29    -- leap year
    |> day
    --> 29

    date 2015 2 28
    |> setDay 29    -- clamped
    |> day
    --> 28

-}
setDay : Int -> Date -> Date
setDay newDay (Date d) =
    firstValid d.year d.month (clampDay newDay)


{-| addYears adds a relative number (positive or negative) of years to
a Date, ensuring that the return value represents a valid Date. If
the new date is not valid, days are subtracted from it until a valid
Date can be produced.

    date 2000 2 29
    |> addYears -1  -- will no longer be leap year
    |> day
    --> 28

-}
addYears : Int -> Date -> Date
addYears years (Date d) =
    firstValid (d.year + years) d.month d.day


{-| addMonths adds a relative number (positive or negative) of months to
a Date, ensuring that the return value represents a valid Date. Its
semantics are the same as `addYears`.

    date 2018 3 31
    |> addMonths -1 -- Switch to Feb
    |> day
    --> 28

-}
addMonths : Int -> Date -> Date
addMonths months (Date d) =
    let
        ms =
            d.year * 12 + monthToInt d.month - 1 + months

        yo =
            if ms < 0 then
                -1

            else
                0
    in
    date (((ms - yo) // 12) + yo) (modBy 12 ms + 1) d.day


{-| days adds an exact number (positive or negative) of days to a
Date. Adding or subtracting days always produces a valid Date so
there is no fuzzing logic here like there is in `add{Months,Years}`.

    date 2018 2 28
    |> addDays 1
    |> month
    --> 3 -- March

-}
addDays : Int -> Date -> Date
addDays days (Date d) =
    daysFromYearMonthDay d.year d.month d.day
        |> (+) days
        |> dateFromDays


{-| compare two Dates.

Note: since this conflicts with **Basics.compare**, have to
preface with **Time.Date.**; see this example:

    date 2018 1 28
    |> addYears -1
    |> addMonths 1
    |> Time.Date.compare (date 2017 2 29)
    --> EQ

-}
compare : Date -> Date -> Order
compare d1 d2 =
    Basics.compare (toTuple d1) (toTuple d2)


{-| delta returns the relative number of years, months and days between two Dates.

Each field is accumulative by itself. That is, `days` not only shows the
difference caused by the two `day` entries in the two `Date` arguments, but
also the added days caused by differences in `months` and `years`. For `months`
and `years`, is the count across month and year change boundaries respectively; illustrated
by last example below.

    -- 3 examples showing that, if the `year` and `month`
    -- are the same in the two `Date` values, then the
    -- `years` and `months` result values remain constant
    -- in spite of large differences in the two inputs'
    -- `day` setting:

    delta (date 2019 1 1) (date 2018 1 1)
    --> { years = 1
    --> , months = 12
    --> , days = 365
    --> }

    delta (date 2019 1 31) (date 2018 1 1)
    --> { years = 1
    --> , months = 12
    --> , days = 395
    --> }

    delta (date 2019 1 1) (date 2018 1 31)
    --> { years = 1
    --> , months = 12
    --> , days = 335
    --> }

    -- 1 day apart but from last day of year to first
    -- day of next year:

    delta (date 2019 1 1) (date 2018 12 31)
    --> { years = 1
    --> , months = 1
    --> , days = 1
    --> }

-}
delta : Date -> Date -> DateDelta
delta (Date d1) (Date d2) =
    { years = d1.year - d2.year
    , months = (abs d1.year * 12 + monthToInt d1.month) - (abs d2.year * 12 + monthToInt d2.month)
    , days =
        daysFromYearMonthDay d1.year d1.month d1.day
            - daysFromYearMonthDay d2.year d2.month d2.day
    }


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


{-| toTuple converts a Date value into a (year, month, day) tuple.
This is useful if you want to use Dates as Dict keys.

    date 2018 5 26
    |> toTuple
    --> (2018, 5, 26)

-}
toTuple : Date -> ( Int, Int, Int )
toTuple (Date d) =
    ( d.year, monthToInt d.month, d.day )


{-| fromTuple converts a (year, month, day) tuple into a Date value.

    (2018, 5, 26)
    |> fromTuple
    --> date 2018 5 26

-}
fromTuple : ( Int, Int, Int ) -> Date
fromTuple ( year_, month_, day_ ) =
    date year_ month_ day_


{-| isValidDate returns True if the given year, month and day
represent a valid date.

NOTE: when you create a Date using `date`, it does not validate
the `year`, `month`, or `day` used; rather it just clamps out-of-range
values to "legal" values without notifying you. If you are worried
about complete validation, pass the 3 values to this
method first and it will validate it. This gives you a chance to
abort creating a "bad" `Date`.

    isValidDate 2016 12 31
    --> True

    isValidDate 2016 12 32
    --> False

    isValidDate 2016 2 29 -- leap year
    --> True

    isValidDate 2018 2 29 -- not leap year
    --> False

-}
isValidDate : Int -> Int -> Int -> Bool
isValidDate year_ month_ day_ =
    day_ >= 1 && day_ <= daysInMonth year_ month_


{-| isLeapYear returns True if the given year is a leap year. The
rules for leap years are as follows:

  - A year that is a multiple of 400 is a leap year.

  - A year that is a multiple of 100 but not of 400 is not a leap year.

  - A year that is a multiple of 4 but not of 100 is a leap year.

    isLeapYear 2016
    --> True

    isLeapYear 2018
    --> False

    isLeapYear 400
    --> True

    isLeapYear 500
    --> False

    isLeapYear (500 + 4)
    --> True

-}
isLeapYear : Int -> Bool
isLeapYear y =
    modBy 400 y == 0 || modBy 100 y /= 0 && modBy 4 y == 0


{-| daysInMonth returns the number of days in a month given a specific
year, taking leap years into account.

  - A regular year has 365 days and the corresponding February has 28 days.

  - A leap year has 366 days and the corresponding February has 29 days.

    daysInMonth 2016 Feb
    --> Just 29

    daysInMonth 2018 Feb
    --> Just 28

-}
daysInMonth : Int -> Int -> Int
daysInMonth y m =
    case clampMonth m of
        Feb ->
            if isLeapYear y then
                29

            else
                28

        Apr ->
            30

        Jun ->
            30

        Sep ->
            30

        Nov ->
            30

        _ ->
            31


firstValid : Int -> Month -> Int -> Date
firstValid year_ month_ day_ =
    let
        monthInt =
            monthToInt month_

        ( y, m, d ) =
            if isValidDate year_ monthInt day_ then
                ( year_, month_, day_ )

            else if isValidDate year_ monthInt (day_ - 1) then
                ( year_, month_, day_ - 1 )

            else if isValidDate year_ monthInt (day_ - 2) then
                ( year_, month_, day_ - 2 )

            else
                ( year_, month_, day_ - 3 )
    in
    Date { year = y, month = m, day = d }


daysFromYearMonthDay : Int -> Month -> Int -> Int
daysFromYearMonthDay year_ month_ day_ =
    let
        yds =
            daysFromYear year_

        mds =
            daysFromYearMonth year_ month_

        dds =
            day_ - 1
    in
    yds + mds + dds


daysFromYearMonth : Int -> Month -> Int
daysFromYearMonth year_ month_ =
    let
        go : Int -> Maybe Month -> Int -> Int
        go y m acc =
            case m of
                Nothing ->
                    acc

                Just m_ ->
                    go y (prevMonth m_) (acc + daysInMonth y (monthToInt m_))
    in
    go year_ (prevMonth month_) 0


prevMonth : Month -> Maybe Month
prevMonth m =
    case m of
        Jan ->
            Nothing

        Feb ->
            Just Jan

        Mar ->
            Just Feb

        Apr ->
            Just Mar

        May ->
            Just Apr

        Jun ->
            Just May

        Jul ->
            Just Jun

        Aug ->
            Just Jul

        Sep ->
            Just Aug

        Oct ->
            Just Sep

        Nov ->
            Just Oct

        Dec ->
            Just Nov


daysFromYear : Int -> Int
daysFromYear y =
    if y > 0 then
        366
            + ((y - 1) * 365)
            + ((y - 1) // 4)
            - ((y - 1) // 100)
            + ((y - 1) // 400)

    else if y < 0 then
        (y * 365)
            + (y // 4)
            - (y // 100)
            + (y // 400)

    else
        0


yearFromDays : Int -> Int
yearFromDays ds =
    let
        y =
            ds // 365

        d =
            daysFromYear y
    in
    if ds <= d then
        y - 1

    else
        y


dateFromDays : Int -> Date
dateFromDays ds =
    let
        d400 =
            daysFromYear 400

        y400 =
            ds // d400

        d =
            remainderBy d400 ds

        year_ =
            yearFromDays (d + 1)

        leap =
            if isLeapYear year_ then
                (+) 1

            else
                identity

        doy =
            d - daysFromYear year_

        ( month_, day_ ) =
            if doy < 31 then
                ( Jan, doy + 1 )

            else if doy < leap 59 then
                ( Feb, doy - 31 + 1 )

            else if doy < leap 90 then
                ( Mar, doy - leap 59 + 1 )

            else if doy < leap 120 then
                ( Apr, doy - leap 90 + 1 )

            else if doy < leap 151 then
                ( May, doy - leap 120 + 1 )

            else if doy < leap 181 then
                ( Jun, doy - leap 151 + 1 )

            else if doy < leap 212 then
                ( Jul, doy - leap 181 + 1 )

            else if doy < leap 243 then
                ( Aug, doy - leap 212 + 1 )

            else if doy < leap 273 then
                ( Sep, doy - leap 243 + 1 )

            else if doy < leap 304 then
                ( Oct, doy - leap 273 + 1 )

            else if doy < leap 334 then
                ( Nov, doy - leap 304 + 1 )

            else
                ( Dec, doy - leap 334 + 1 )
    in
    Date
        { year = year_ + y400 * 400
        , month = month_
        , day = day_
        }


clampMonth : Int -> Month
clampMonth month_ =
    case month_ of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


clampDay : Int -> Int
clampDay day_ =
    clamp 1 31 day_
