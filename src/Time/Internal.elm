module Time.Internal exposing (..)

import Combine exposing (..)
import Combine.Num


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
    (clamp 0 23 hour) * hourMs + (clamp 0 59 minute) * minuteMs + (clamp 0 59 second) * secondMs + (clamp 0 999 millisecond)


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


paddedInt : Parser s Int
paddedInt =
    Combine.optional "" (Combine.string "0") *> Combine.Num.int


intRange : Int -> Int -> Parser s Int
intRange lo hi =
    let
        validate n =
            if n >= lo && n <= hi then
                Combine.succeed n
            else
                Combine.fail ("expected an integer in the range [" ++ toString lo ++ ", " ++ toString hi ++ "]")
    in
        paddedInt >>= validate
