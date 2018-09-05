module TestTimeZone exposing (unpackTimeZoneData)

import Debug
import Expect exposing (Expectation)
import Test exposing (..)
import Time.TimeZoneData exposing (africa_bissau_l, unpack)
import Time.TimeZones exposing (europe_bucharest)
import Time.ZonedDateTime exposing (..)


unpackTimeZoneData : Test
unpackTimeZoneData =
    describe "Time.TimeZone.unpack"
        [ test "unpack converts a packed string into a TimeZone" <|
            \() ->
                let
                    tz =
                        unpack "Africa/Bissau|LMT WAT GMT|12.k 10 0|012|-2ldWV.E 2xonV.E|39e4"
                in
                Expect.equal africa_bissau_l tz
        ]
