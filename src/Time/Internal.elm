module Time.Internal exposing (..)

import Char
import Parser exposing
    ( Parser, Count(..), andThen, fail, ignore, inContext
    , keep, succeed, zeroOrMore
    )


type alias DateTimeData =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }


type alias TimeData d =
    { d
        | hour : Int
        , minute : Int
        , second : Int
        , millisecond : Int
    }


offsetFromTimeData : TimeData d -> Int
offsetFromTimeData { hour, minute, second, millisecond } =
    clamp 0 23 hour * hourMs + clamp 0 59 minute * minuteMs + clamp 0 59 second * secondMs + clamp 0 999 millisecond


zero : DateTimeData
zero =
    { year = 0
    , month = 1
    , day = 1
    , hour = 0
    , minute = 0
    , second = 0
    , millisecond = 0
    }


padded : Int -> String
padded n =
    if n < 10 then
        "0" ++ toString n
    else
        toString n


padded3 : Int -> String
padded3 n =
    String.padLeft 3 '0' (toString n)


dayMs : number
dayMs =
    86400000


hourMs : number
hourMs =
    3600000


minuteMs : number
minuteMs =
    60000


secondMs : number
secondMs =
    1000



-- Shared parsers
-- --------------


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
                    ("expected the value "
                        ++ toString n
                        ++ " to be an integer in the range "
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
