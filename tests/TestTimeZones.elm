module TestTimeZones exposing (..)

import Dict
import Expect exposing (Expectation)
import List
import Test exposing (..)
import Time.TimeZone
import Time.TimeZones


linkedTimeZoneTest : Test
linkedTimeZoneTest =
    describe "Time.TimeZones"
        [ test "Europe/Zagreb is an alias for Europe/Belgrade" <|
            \() ->
                Time.TimeZones.fromName "Europe/Zagreb"
                    -- Rename the zone data returned so we can compare just
                    -- the `spans`, i.e. the actual time zone data.
                    |> Maybe.map (Time.TimeZone.setName "Europe/Belgrade")
                    |> Expect.equal (Just (Time.TimeZones.europe_belgrade ()))
        ]


namedTimeZoneTest : Test
namedTimeZoneTest =
    describe "Time.TimeZones naming" <|
        let
            check ( timeZoneName, loadTimeZone ) =
                test (timeZoneName ++ "'s TimeZone is named " ++ timeZoneName) <|
                    \() -> loadTimeZone () |> Time.TimeZone.name |> Expect.equal timeZoneName
        in
            Dict.toList Time.TimeZones.all |> List.map check
