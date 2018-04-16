module TestTimeZonePieces exposing (..)

--import Lazy exposing (force)

import Debug
import Expect exposing (Expectation, equal, fail)
import Test exposing (..)
import Time.TimeZone
import Parser exposing (run)


source =
    "Africa/Bissau|LMT -01 GMT|12.k 10 0|012|-2ldWV.E 2xonV.E|39e4"


name : Test
name =
    describe "Time.TimeZone.name"
        [ test "Parse a timezone name" <|
            \() ->
                case run Time.TimeZone.parseName source of
                    Err msg ->
                        fail (toString msg)

                    Ok value ->
                        Expect.equal "Africa/Bissau" value
        , test "In addition, parse the abbreviations" <|
            \() ->
                case run Time.TimeZone.packedTimeZoneTuple source of
                    Err msg ->
                        fail (toString msg)

                    Ok value ->
                        Expect.equal ("Africa/Bissau", ["LMT", "-01", "GMT"]) value
        ]
