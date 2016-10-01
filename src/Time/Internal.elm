module Time.Internal exposing (..)


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


offsetFromTimeData : TimeData d -> Maybe Int
offsetFromTimeData { hour, minute, second, millisecond } =
    if hour >= 0 && hour < 24 && minute >= 0 && minute < 60 && second >= 0 && second < 60 && millisecond >= 0 && millisecond < 1000 then
        Just <| hour * hourMs + minute * minuteMs + second * secondMs + millisecond
    else
        Nothing


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
