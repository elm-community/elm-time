module Time.Iso8601
    exposing
        ( fromDate
        , fromDateTime
        , fromZonedDateTime
        , toDate
        , toDateTime
        , toZonedDateTime
        )

{-|


# Render ISO8601 strings

@docs fromDate, fromDateTime, fromZonedDateTime


# Parse ISO8601 strings

@docs toDate, toDateTime, toZonedDateTime

-}

import Char
import Parser
    exposing
        ( (|.)
        , (|=)
        , Count(..)
        , Error
        , Parser
        , andThen
        , fail
        , ignore
        , inContext
        , keep
        , oneOf
        , oneOrMore
        , run
        , succeed
        , zeroOrMore
        )
import Time.Date as Date
    exposing
        ( Date
        , daysInMonth
        , isValidDate
        )
import Time.DateTime as DateTime
    exposing
        ( DateTime
        , addMilliseconds
        , dateTime
        , day
        , hour
        , makeDateTime
        , millisecond
        , minute
        , month
        , second
        , year
        )
import Time.Internal
    exposing
        ( hourMs
        , minuteMs
        , offsetFromTimeData
        , padded
        , padded3
        )
import Time.TimeZone exposing (TimeZone)
import Time.ZonedDateTime as ZDT exposing (ZonedDateTime)


-- Shared parsers
-- --------------


{-| Offset is expressed in +/- milliseconds
-}
type alias Milliseconds =
    Int


{-| fromDate renders a Date in ISO8601 format.

    import Time.Date exposing (..)

    date 2018 5 27
    |> fromDate
    --> "2018-05-27"

-}
fromDate : Date -> String
fromDate date =
    (toString (Date.year date) |> String.padLeft 4 '0')
        ++ "-"
        ++ padded (Date.month date)
        ++ "-"
        ++ padded (Date.day date)


{-| fromDateTime renders a DateTime in ISO8601 format.

    import Time.DateTime exposing (..)

    epoch
    |> addMilliseconds 61000
    |> fromDateTime
    --> "1970-01-01T00:01:01.000Z"

-}
fromDateTime : DateTime -> String
fromDateTime time =
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
        ++ "."
        ++ padded3 (millisecond time)
        ++ "Z"


{-| fromZonedDateTime renders a ZonedDateTime in ISO8601 format.

    import Time.ZonedDateTime
    import Time.TimeZones exposing (america_new_york)
    import Time.DateTime exposing (epoch)

    Time.ZonedDateTime.fromDateTime (america_new_york ()) epoch
    |> fromZonedDateTime
    --> "1969-12-31T19:00:00.000-05:00"

-}
fromZonedDateTime : ZonedDateTime -> String
fromZonedDateTime dateTime =
    toString (ZDT.year dateTime)
        ++ "-"
        ++ padded (ZDT.month dateTime)
        ++ "-"
        ++ padded (ZDT.day dateTime)
        ++ "T"
        ++ padded (ZDT.hour dateTime)
        ++ ":"
        ++ padded (ZDT.minute dateTime)
        ++ ":"
        ++ padded (ZDT.second dateTime)
        ++ "."
        ++ padded3 (ZDT.millisecond dateTime)
        ++ ZDT.utcOffsetString dateTime


{-| toZonedDateTime parses an ISO8601-formatted string into a
ZonedDateTime object, adjusting for its offset.

    import Time.ZonedDateTime
    import Time.TimeZones exposing (america_new_york)
    import Time.DateTime exposing (epoch)

    toZonedDateTime (america_new_york ()) "1970-01-01T00:00:00.000Z"
    --> Ok (Time.ZonedDateTime.fromDateTime (america_new_york ()) epoch)

-}
toZonedDateTime : TimeZone -> String -> Result Parser.Error ZonedDateTime
toZonedDateTime timeZone input =
    toDateTime input
        |> Result.map (ZDT.fromDateTime timeZone)


{-| toDate parses an ISO8601-formatted date string into a Date.

    import Time.Date

    toDate "1970-12-01"
    --> Ok (Time.Date.date 1970 12 1)

    toDate "19701201"
    --> Ok (Time.Date.date 1970 12 1)

-}
toDate : String -> Result Parser.Error Date
toDate input =
    run parseDate input


{-| -}
parseDate : Parser Date
parseDate =
    (succeed (,,)
        |= parseYear
        |. optional '-'
        |= parseMonth
        |. optional '-'
        |= parseDay
    )
        |> andThen convertDate


parseYear : Parser Int
parseYear =
    digits "year" 4


parseMonth : Parser Int
parseMonth =
    digitsInRange "month" 2 1 12


parseDay : Parser Int
parseDay =
    digitsInRange "day-in-month" 2 1 31


digits : String -> Int -> Parser Int
digits name digitsCount =
    inContext name <|
        (keep (Exactly digitsCount) Char.isDigit
            |> andThen (fromResult << String.toInt)
        )


convertDate : ( Int, Int, Int ) -> Parser Date
convertDate ( year, month, day ) =
    if isValidDate year month day then
        succeed (Date.date year month day)
    else
        complainInvalid ( year, month, day )


complainInvalid : ( Int, Int, Int ) -> Parser Date
complainInvalid ( year, month, day ) =
    inContext "leap-year" <|
        let
            maxDays =
                Maybe.withDefault 31 (daysInMonth year month)

            msg =
                "Expecting the value "
                    ++ toString day
                    ++ " to be in the range 1 to "
                    ++ toString maxDays
                    ++ " for the specified year, "
                    ++ toString year
                    ++ ", and month, "
                    ++ toString month
                    ++ "."
        in
            fail msg


digitsInRange : String -> Int -> Int -> Int -> Parser Int
digitsInRange name digitsCount lo hi =
    inContext name <|
        (keep (Exactly digitsCount) (\c -> Char.isDigit c)
            |> andThen (intRange lo hi << String.toInt)
        )


intRange : Int -> Int -> Result String Int -> Parser Int
intRange lo hi result =
    case result of
        Ok n ->
            if n >= lo && n <= hi then
                succeed n
            else
                fail
                    ("Expecting the value "
                        ++ toString n
                        ++ " to be in the range "
                        ++ toString lo
                        ++ " to "
                        ++ toString hi
                        ++ "."
                    )

        Err msg ->
            Parser.fail msg


fromResult : Result String Int -> Parser Int
fromResult result =
    case result of
        Ok i ->
            succeed i

        Err msg ->
            fail msg


optional : Char -> Parser ()
optional char =
    ignore zeroOrMore (\c -> c == char)


{-| toDateTime parses an ISO8601-formatted date time string into a
DateTime object, adjusting for its timezone offset.
-}
toDateTime : String -> Result Parser.Error DateTime
toDateTime input =
    run parseDateTime input


parseDateTime : Parser DateTime
parseDateTime =
    (succeed (,,)
        |= parseDate
        |. optional 'T'
        |= parseOffset
        |= tZOffset
    )
        |> andThen convertDateTime


parseOffset : Parser Milliseconds
parseOffset =
    (succeed (,,,)
        |= digitsInRange "hours" 2 0 23
        |. optional ':'
        |= digitsInRange "minutes" 2 0 59
        |. optional ':'
        |= digitsInRange "seconds" 2 0 59
        |= fraction
    )
        |> andThen convertTime


convertDateTime : ( Date, Milliseconds, Milliseconds ) -> Parser DateTime
convertDateTime ( date, offset, tZOffset ) =
    succeed
        (makeDateTime
            date
            offset
            |> addMilliseconds tZOffset
        )


convertTime : ( Int, Int, Int, Int ) -> Parser Milliseconds
convertTime ( hours, minutes, seconds, milliseconds ) =
    succeed
        (offsetFromTimeData
            { hour = hours
            , minute = minutes
            , second = seconds
            , millisecond = milliseconds
            }
        )


fraction : Parser Milliseconds
fraction =
    oneOf
        [ optionalFraction
        , succeed 0
        ]


optionalFraction : Parser Milliseconds
optionalFraction =
    inContext "fraction" <|
        ((succeed identity
            |. keep (Exactly 1) ((==) '.')
            |= keep oneOrMore Char.isDigit
         )
            |> andThen (fromResult << getFraction)
        )


getFraction : String -> Result String Milliseconds
getFraction fractionString =
    let
        numerator =
            Result.withDefault 0 (String.toInt fractionString)

        denominator =
            10 ^ String.length fractionString
    in
        Ok (round (Time.Internal.secondMs * toFloat numerator / toFloat denominator))


tZOffset : Parser Milliseconds
tZOffset =
    oneOf
        [ utc
        , optionalTZOffset
        , succeed 0
        ]


utc : Parser Milliseconds
utc =
    (succeed identity
        |. keep (Exactly 1) ((==) 'Z')
    )
        |> andThen (fromResult << (\_ -> Ok 0))


optionalTZOffset : Parser Milliseconds
optionalTZOffset =
    inContext "offset" <|
        ((succeed (,,)
            |= polarity
            |= digitsInRange "timezone hours" 2 0 23
            |. optional ':'
            |= digitsInRange "timezone minutes" 2 0 59
         )
            |> andThen (fromResult << getTZOffset)
        )


polarity : Parser Int
polarity =
    inContext "timezone polarity" <|
        (keep (Exactly 1)
            (\c ->
                c
                    == '+'
                    || c
                    == '-'
                    || c
                    == '−'
             --U+2212
            )
            |> andThen
                (fromResult
                    << -- Code has to do opposite of sign char
                       (\sign ->
                            if sign == "+" then
                                Ok -1
                            else
                                Ok 1
                       )
                )
        )


getTZOffset : ( Int, Int, Int ) -> Result String Milliseconds
getTZOffset ( polarity, hrs, min ) =
    Ok
        (polarity
            * hrs
            * hourMs
            + polarity
            * min
            * minuteMs
        )
