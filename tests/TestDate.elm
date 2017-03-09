module TestDate exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (int, intRange)
import Test exposing (..)
import Time.Date exposing (..)


someDate : Date
someDate =
    date 1992 5 29


datesEqual : Date -> ( Int, Int, Int ) -> Expect.Expectation
datesEqual date dateTuple =
    Expect.equal (toTuple date) dateTuple


validLeapYears : List Int
validLeapYears =
    List.concat [ [ -400, -396, -392, -388, -384, -380, -376, -372, -368, -364, -360, -356, -352, -348, -344, -340, -336, -332, -328, -324, -320, -316, -312, -308, -304, -296, -292, -288, -284, -280 ], [ -276, -272, -268, -264, -260, -256, -252, -248, -244, -240, -236, -232, -228, -224, -220, -216, -212, -208, -204, -196, -192, -188, -184, -180, -176, -172, -168, -164, -160, -156 ], [ -152, -148, -144, -140, -136, -132, -128, -124, -120, -116, -112, -108, -104, -96, -92, -88, -84, -80, -76, -72, -68, -64, -60, -56, -52, -48, -44, -40, -36, -32 ], [ -28, -24, -20, -16, -12, -8, -4, 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88 ], [ 92, 96, 104, 108, 112, 116, 120, 124, 128, 132, 136, 140, 144, 148, 152, 156, 160, 164, 168, 172, 176, 180, 184, 188, 192, 196, 204, 208, 212, 216 ], [ 220, 224, 228, 232, 236, 240, 244, 248, 252, 256, 260, 264, 268, 272, 276, 280, 284, 288, 292, 296, 304, 308, 312, 316, 320, 324, 328, 332, 336, 340 ], [ 344, 348, 352, 356, 360, 364, 368, 372, 376, 380, 384, 388, 392, 396, 400, 404, 408, 412, 416, 420, 424, 428, 432, 436, 440, 444, 448, 452, 456, 460 ], [ 464, 468, 472, 476, 480, 484, 488, 492, 496, 504, 508, 512, 516, 520, 524, 528, 532, 536, 540, 544, 548, 552, 556, 560, 564, 568, 572, 576, 580, 584 ], [ 588, 592, 596, 604, 608, 612, 616, 620, 624, 628, 632, 636, 640, 644, 648, 652, 656, 660, 664, 668, 672, 676, 680, 684, 688, 692, 696, 704, 708, 712 ], [ 716, 720, 724, 728, 732, 736, 740, 744, 748, 752, 756, 760, 764, 768, 772, 776, 780, 784, 788, 792, 796, 800, 804, 808, 812, 816, 820, 824, 828, 832 ], [ 836, 840, 844, 848, 852, 856, 860, 864, 868, 872, 876, 880, 884, 888, 892, 896, 904, 908, 912, 916, 920, 924, 928, 932, 936, 940, 944, 948, 952, 956 ], [ 960, 964, 968, 972, 976, 980, 984, 988, 992, 996, 1004, 1008, 1012, 1016, 1020, 1024, 1028, 1032, 1036, 1040, 1044, 1048, 1052, 1056, 1060, 1064, 1068, 1072, 1076, 1080 ], [ 1084, 1088, 1092, 1096, 1104, 1108, 1112, 1116, 1120, 1124, 1128, 1132, 1136, 1140, 1144, 1148, 1152, 1156, 1160, 1164, 1168, 1172, 1176, 1180, 1184, 1188, 1192, 1196, 1200, 1204 ], [ 1208, 1212, 1216, 1220, 1224, 1228, 1232, 1236, 1240, 1244, 1248, 1252, 1256, 1260, 1264, 1268, 1272, 1276, 1280, 1284, 1288, 1292, 1296, 1304, 1308, 1312, 1316, 1320, 1324, 1328 ], [ 1332, 1336, 1340, 1344, 1348, 1352, 1356, 1360, 1364, 1368, 1372, 1376, 1380, 1384, 1388, 1392, 1396, 1404, 1408, 1412, 1416, 1420, 1424, 1428, 1432, 1436, 1440, 1444, 1448, 1452 ], [ 1456, 1460, 1464, 1468, 1472, 1476, 1480, 1484, 1488, 1492, 1496, 1504, 1508, 1512, 1516, 1520, 1524, 1528, 1532, 1536, 1540, 1544, 1548, 1552, 1556, 1560, 1564, 1568, 1572, 1576 ], [ 1580, 1584, 1588, 1592, 1596, 1600, 1604, 1608, 1612, 1616, 1620, 1624, 1628, 1632, 1636, 1640, 1644, 1648, 1652, 1656, 1660, 1664, 1668, 1672, 1676, 1680, 1684, 1688, 1692, 1696 ], [ 1704, 1708, 1712, 1716, 1720, 1724, 1728, 1732, 1736, 1740, 1744, 1748, 1752, 1756, 1760, 1764, 1768, 1772, 1776, 1780, 1784, 1788, 1792, 1796, 1804, 1808, 1812, 1816, 1820, 1824 ], [ 1828, 1832, 1836, 1840, 1844, 1848, 1852, 1856, 1860, 1864, 1868, 1872, 1876, 1880, 1884, 1888, 1892, 1896, 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948 ], [ 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020 ] ]


standardYearMonths : List Int
standardYearMonths =
    [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


leapYearMonths : List Int
leapYearMonths =
    [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


monthDays : Int -> Int -> Int
monthDays year month =
    let
        monthDays =
            if isLeapYear year then
                leapYearMonths
            else
                standardYearMonths
    in
        monthDays
            |> List.drop (month - 1)
            |> List.head
            |> Maybe.withDefault 0


fuzzDate : String -> (Int -> Int -> Int -> Expectation) -> Test
fuzzDate =
    fuzz3 int (intRange 1 12) (intRange 1 31)


constructing : Test
constructing =
    describe "Time.Date.date"
        [ fuzzDate "constructs dates" <|
            \year month day ->
                let
                    day_ =
                        clamp 1 day (monthDays year month)
                in
                    datesEqual (date year month day_) ( year, month, day_ )
        , test "constructs valid dates" <|
            always <|
                datesEqual (date 1992 5 29) ( 1992, 5, 29 )
        , test "accounts for leap years" <|
            always <|
                datesEqual (date 1992 2 29) ( 1992, 2, 29 )
        , test "clamps invalid dates" <|
            always <|
                datesEqual (date 1993 2 29) ( 1993, 2, 28 )
        ]


leapYears : Test
leapYears =
    describe "Time.Date.isLeapYear"
        [ describe "isLeapYear"
            [ fuzz (intRange -400 2020) "is correct given any year" <|
                \year ->
                    if List.member year validLeapYears then
                        Expect.true ("Expected " ++ toString year ++ " to be a leap year") (isLeapYear year)
                    else
                        Expect.false (toString year ++ " is not a leap year") (isLeapYear year)
            ]
        , describe "daysInMonth"
            [ fuzz2 (intRange -400 2020) (intRange 1 12) "is correct given any year, month pair" <|
                \year month ->
                    daysInMonth year month
                        |> Expect.equal (Just <| monthDays year month)
            ]
        ]


adders : Test
adders =
    describe "Time.Date.add{Years,Months,Days}"
        [ test "addYears is relative" <|
            \() ->
                let
                    date1 =
                        date 1992 2 29
                            |> addYears 1

                    date2 =
                        date 1993 2 28
                in
                    Expect.equal date1 date2
        , test "addMonths is relative" <|
            \() ->
                let
                    date1 =
                        date 1992 1 31
                            |> addMonths 1

                    date2 =
                        date 1992 2 29
                in
                    Expect.equal date1 date2
        , fuzz3 (intRange -400 3000) (intRange 1 12) (intRange -100 100) "addMonths is revertable" <|
            \year month addage ->
                let
                    date1 =
                        date year month 1

                    date2 =
                        addMonths addage date1
                in
                    Expect.equal date1 (addMonths -addage date2)
        , fuzz int "addDays is absolute" <|
            \days ->
                let
                    date1 =
                        someDate

                    date2 =
                        addDays days date1

                    dt =
                        delta date2 date1
                in
                    Expect.equal days dt.days
        ]


toFromTuple : Test
toFromTuple =
    describe "Time.Date.{to,from}Tuple"
        [ fuzzDate "they have an inverse relationship" <|
            \year month day ->
                let
                    date1 =
                        date year month day

                    date2 =
                        date1
                            |> toTuple
                            |> fromTuple
                in
                    Expect.equal date1 date2
        ]


toFromISO8601 : Test
toFromISO8601 =
    let
        renderEq date output () =
            toISO8601 date
                |> Expect.equal output

        parseEq input date () =
            case ( fromISO8601 input, date ) of
                ( Err message, _ ) ->
                    Expect.fail (message ++ " in input '" ++ input ++ "'")

                ( Ok date1, date2 ) ->
                    if date1 == date2 then
                        Expect.pass
                    else
                        Expect.fail ("expected '" ++ toISO8601 date1 ++ "' to equal '" ++ toISO8601 date2 ++ "' from input '" ++ input ++ "'")

        parseFails input () =
            case fromISO8601 input of
                Err _ ->
                    Expect.pass

                Ok _ ->
                    Expect.fail ("parsing '" ++ input ++ "' should have failed")
    in
        describe "Time.Date.{to,from}ISO8601"
            [ test "toISO8601 of epoch is correct" <|
                renderEq (date 1970 1 1) "1970-01-01"
            , test "toISO8601 of some date is correct" <|
                renderEq someDate "1992-05-29"
            , test "fromISO8601 of a valid date is correct" <|
                parseEq "1992-05-29" (date 1992 5 29)
            , test "fromISO8601 of a badly-formatted date fails" <|
                parseFails ""
            , test "fromISO8601 of a badly-formatted date fails 2" <|
                parseFails "1992-05"
            , test "fromISO8601 of an invalid date fails" <|
                parseFails "1991-02-31"
            ]


all : Test
all =
    describe "Time.Date"
        [ constructing
        , leapYears
        , adders
        , toFromISO8601
        , toFromTuple
        ]
