module TestTimeZone exposing (..)

import Debug
import Lazy exposing (force)
import Expect exposing (Expectation)
import Test exposing (..)
--import Time.TimeZone exposing (unpack)
import Time.TimeZones exposing (europe_bucharest)
import Time.TimeZoneData exposing (unpack, africa_bissau_l)
import Time.ZonedDateTime exposing (..)


unpackTimeZoneData : Test
unpackTimeZoneData =
    describe "Time.TimeZone.unpack"
        [ test "unpack converts a packed string into a TimeZone" <|
            \() ->
                let
                    tz =
                        force (unpack "Africa/Bissau|LMT -01 GMT|12.k 10 0|012|-2ldWV.E 2xonV.E|39e4")
                in
                    Expect.equal (force africa_bissau_l) tz
        ]
