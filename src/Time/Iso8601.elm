module Time.Iso8601 exposing (toDateTime)

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
import Time.Date exposing (Date, date, daysInMonth, isValidDate)
import Time.DateTime
    exposing
        ( DateTime
        , addMilliseconds
        , dateTime
        , makeDateTime
        )
import Time.Internal exposing (hourMs, minuteMs, offsetFromTimeData)


-- Shared parsers
-- --------------


{-| Offset is expressed in +/- milliseconds
-}
type alias Milliseconds =
    Int


{-| fromISO8601Date parses an ISO8601-formatted date string into a Date.
-}
fromISO8601Date : String -> Result Parser.Error Date
fromISO8601Date input =
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
        succeed (date year month day)
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
