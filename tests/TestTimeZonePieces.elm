module TestTimeZonePieces exposing (..)

--import Lazy exposing (force)

import Debug
import Expect exposing (Expectation, equal, fail)
import Test exposing (..)
import Time.TimeZone exposing (TimeZone)
import Parser exposing (run)


source =
    "Africa/Bissau|LMT -01 GMT|12.k 10 0|012|-2ldWV.E 2xonV.E|39e4"


unpackTest : Test
unpackTest =
    describe "Time.TimeZone.unpack"
        [ test "New" <|
            \() ->
                case Time.TimeZone.unpack source of
                    Ok value ->
                        let
                            v =
                                value
                        in
                            Expect.pass

                    Err msg ->
                        fail (toString msg)
        ]
