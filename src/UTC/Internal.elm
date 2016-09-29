module UTC.Internal exposing (..)


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
