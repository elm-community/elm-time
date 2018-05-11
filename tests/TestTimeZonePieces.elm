module TestTimeZonePieces exposing (..)

--import Lazy exposing (force)

import Debug
import Expect exposing (Expectation, equal, fail)
import Test exposing (..)
import Time.TimeZone exposing (TimeZone)
import Parser exposing (run)
import Combine


source =
    "Africa/Bissau|LMT -01 GMT|12.k 10 0|012|-2ldWV.E 2xonV.E|39e4"


expectedAbbrevs =
    [ "LMT", "-01", "GMT" ]


expectedOffsets =
    [ 62.333333333333336, 60, 0 ]


expectedIndices =
    [ 0, 1, 2 ]


expectedDiffs =
    [ -1830380260000, 1988150260000 ]


timezone =
    { name = "Africa/Bissau"
    , spans =
        [ { from = -1 / 0
          , until = -1830380260000
          , abbreviation = "LMT"
          , offset = 3740000
          }
        , { from = -1830380260000
          , until = 157770000000
          , abbreviation = "-01"
          , offset = 3600000
          }
        , { from = 157770000000
          , until = 1 / 0
          , abbreviation = "GMT"
          , offset = 0
          }
        ]
    }


packedTimeZoneTupleTest : Test
packedTimeZoneTupleTest =
    describe "Time.TimeZone.packedTimeZoneTuple"
        [ test "New" <|
            \() ->
                case run Time.TimeZone.packedTimeZoneTupleNew source of
                    Err msg ->
                        fail (toString msg)

                    Ok value ->
                        Expect.equal
                            ( "Africa/Bissau"
                            , expectedAbbrevs
                            , expectedOffsets
                            , expectedIndices
                            , expectedDiffs
                            )
                            value
        ]


unpackNewTest : Test
unpackNewTest =
    describe "Time.TimeZone.unpackNew"
        [ test "New" <|
            \() ->
                case Time.TimeZone.unpack source of
                    Ok value ->
                        let
                            v =
                                Debug.log "TimeZone" value
                        in
                            Expect.pass

                    Err msg ->
                        fail (toString msg)
        ]
