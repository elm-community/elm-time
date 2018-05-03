module Time.TimeZone
    exposing
        ( TimeZone
        , name
        , abbreviation
        , offset
        , offsetString
        , setName
        , unpack
        , parseName
        , parseAbbrevs
        , packedTimeZoneTupleNew
        , packedTimeZoneTupleOld
        )

{-| This module defines a representations for Timezone information.


# TimeZone values

@docs TimeZone, name, abbreviation, offset, offsetString


# Manipulating TimeZones

@docs setName


# Constructing TimeZones

@docs unpack


# Temporary

@docs parseName, parseAbbrevs, packedTimeZoneTupleNew, packedTimeZoneTupleOld

-}

import Char
import Debug
    exposing
        ( log
        )
import Parser as ParserNew
    exposing
        ( (|.)
        , (|=)
        , Count(..)
        , delayedCommit
        , ignore
        , keep
        , oneOf
        , oneOrMore
        , run
        , zeroOrMore
        )
import Combine exposing (..)
import Combine.Num
import Time exposing (Time)
import Time.Internal exposing (..)


{-| TimeZone represents the opaque type of timezone values. These are
generally loaded from an external source via `unpack`.

See also <http://momentjs.com/timezone/docs/#/data-formats/packed-format/>.

-}
type TimeZone
    = TimeZone
        { name : String
        , spans : List Span
        }


{-| Spans represent variations within a TimeZone. A Time has an
associated Span if `.from <= t < .until`.

`offset` is the Span's UTC offset in milliseconds.

-}
type alias Span =
    { from : Time
    , until : Time
    , abbreviation : String
    , offset : Int
    }


{-| Given an arbitrary Time and a TimeZone, abbreviation returns the
TimeZone's abbreviation at that Time.
-}
abbreviation : Time -> TimeZone -> String
abbreviation time (TimeZone { spans }) =
    find time spans |> .abbreviation


{-| Given an arbitrary Time and a TimeZone, offset returns the
TimeZone's UTC offset in milliseconds at that Time.
-}
offset : Time -> TimeZone -> Int
offset time (TimeZone { spans }) =
    find time spans |> .offset


{-| Given an arbitrary Time and TimeZone, offsetString returns an
ISO8601-formatted UTC offset for at that Time.
-}
offsetString : Time -> TimeZone -> String
offsetString time timeZone =
    let
        utcOffset =
            offset time timeZone // minuteMs

        hours =
            abs utcOffset // 60

        minutes =
            abs utcOffset % 60

        string =
            padded hours ++ ":" ++ padded minutes
    in
        if utcOffset <= 0 then
            "+" ++ string
        else
            "-" ++ string


find : Time -> List Span -> Span
find time spans =
    let
        go xs =
            case xs of
                [] ->
                    Debug.crash "find: invalid span list"

                x :: xs ->
                    if time >= x.from && time < x.until then
                        x
                    else
                        go xs
    in
        go spans


{-| setName updates a TimeZone's name.
-}
setName : String -> TimeZone -> TimeZone
setName name (TimeZone tz) =
    TimeZone { tz | name = name }


{-| name returns a TimeZone's name.
-}
name : TimeZone -> String
name (TimeZone { name }) =
    name


{-| unpack decodes a packed zone data object into a TimeZone value.

See also <http://momentjs.com/timezone/docs/#/data-formats/packed-format/>

-}
unpack : String -> Result (List String) TimeZone
unpack data =
    case Combine.parse packedTimeZone data of
        Ok ( _, _, zone ) ->
            Ok zone

        Err ( _, _, errors ) ->
            Err errors


{-| unpackNew decodes a packed zone data object into a TimeZone value.

See also <http://momentjs.com/timezone/docs/#/data-formats/packed-format/>

-}
unpackNew : String -> Result ParserNew.Error TimeZone
unpackNew data =
    run packedTimeZoneNew data


type alias PackedTimeZone =
    { name : String
    , abbrevs : List String
    , offsets : List Float
    , indices : List Int
    , diffs : List Float
    }


{-| packedTimeZoneTupleOld parses a zone data string into a TimeZone, validating that
the data format invariants hold.
-}
packedTimeZoneTupleOld : Parser s ( String, List String, List Float, List Int, List Float )
packedTimeZoneTupleOld =
    let
        name =
            Combine.regex "[^|]+"
                <* Combine.string "|"

        abbrevs =
            Combine.sepBy1 (Combine.string " ") (Combine.regex "[^ |]+")
                <* Combine.string "|"

        offsets =
            Combine.sepBy1 (Combine.string " ") base60
                <* Combine.string "|"

        indices =
            (\s -> List.map (\n -> floor <| unsafeBase60 1 n "") (String.split "" s))
                <$> Combine.regex "[^|]+"
                <* Combine.string "|"

        diffs =
            List.map ((*) 60000)
                <$> Combine.sepBy (Combine.string " ") base60

        decode =
            PackedTimeZone
                <$> name
                <*> abbrevs
                <*> offsets
                <*> indices
                <*> diffs

        validate data =
            let
                abbrevs =
                    List.length data.abbrevs

                offsets =
                    List.length data.offsets

                maxIndex =
                    List.maximum data.indices
                        |> Maybe.withDefault 0
            in
                if abbrevs /= offsets then
                    Combine.fail "abbrevs and offsets have different lengths"
                else if maxIndex >= abbrevs then
                    Combine.fail "highest index is longer than both abbrevs and offsets"
                else
                    Combine.succeed data

        span times data i idx =
            { from = times !! i
            , until = times !! (i + 1)
            , abbreviation = data.abbrevs !! idx
            , offset = round (data.offsets !! idx * minuteMs)
            }

        convert data =
            let
                times =
                    if not <| List.isEmpty data.diffs then
                        List.scanl (+) (data.diffs !! 0) (List.drop 1 data.diffs)
                    else
                        []

                -- surround times with - and +infinity
                paddedTimes =
                    [ -1 / 0 ] ++ times ++ [ 1 / 0 ]
            in
                (data.name
                , data.abbrevs
                , data.offsets
                , data.indices
                , data.diffs
                )
    in
        convert <$> (decode >>= validate)



{-| packedTimeZone parses a zone data string into a TimeZone, validating that
the data format invariants hold.
-}
packedTimeZone : Parser s TimeZone
packedTimeZone =
    let
        name =
            Combine.regex "[^|]+"
                <* Combine.string "|"

        abbrevs =
            Combine.sepBy1 (Combine.string " ") (Combine.regex "[^ |]+")
                <* Combine.string "|"

        offsets =
            Combine.sepBy1 (Combine.string " ") base60
                <* Combine.string "|"

        indices =
            (\s -> List.map (\n -> floor <| unsafeBase60 1 n "") (String.split "" s))
                <$> Combine.regex "[^|]+"
                <* Combine.string "|"

        diffs =
            List.map ((*) 60000)
                <$> Combine.sepBy (Combine.string " ") base60

        decode =
            PackedTimeZone
                <$> name
                <*> abbrevs
                <*> offsets
                <*> indices
                <*> diffs

        validate data =
            let
                abbrevs =
                    List.length data.abbrevs

                offsets =
                    List.length data.offsets

                maxIndex =
                    List.maximum data.indices
                        |> Maybe.withDefault 0
            in
                if abbrevs /= offsets then
                    Combine.fail "abbrevs and offsets have different lengths"
                else if maxIndex >= abbrevs then
                    Combine.fail "highest index is longer than both abbrevs and offsets"
                else
                    Combine.succeed data

        span times data i idx =
            { from = times !! i
            , until = times !! (i + 1)
            , abbreviation = data.abbrevs !! idx
            , offset = round (data.offsets !! idx * minuteMs)
            }

        convert data =
            let
                times =
                    if not <| List.isEmpty data.diffs then
                        List.scanl (+) (data.diffs !! 0) (List.drop 1 data.diffs)
                    else
                        []

                -- surround times with - and +infinity
                paddedTimes =
                    [ -1 / 0 ] ++ times ++ [ 1 / 0 ]
            in
                TimeZone
                    { name = data.name
                    , spans = List.indexedMap (span paddedTimes data) data.indices
                    }
    in
        convert <$> (decode >>= validate)


parseBar : ParserNew.Parser ()
parseBar =
    ignore (Exactly 1) ((==) '|')


parseSpace : ParserNew.Parser ()
parseSpace =
    ignore (Exactly 1) ((==) ' ')


{-| -}
parseName : ParserNew.Parser String
parseName =
    ParserNew.inContext "name" <|
        ParserNew.succeed identity
            |= keep oneOrMore ((/=) '|')


parseAbbrev : ParserNew.Parser String
parseAbbrev =
    keep oneOrMore (\c -> c /= ' ' && c /= '|')


{-| Parse the first abbrev and then use `abbrevsHelp` to find
the remaining ones.
-}
parseAbbrevs : ParserNew.Parser (List String)
parseAbbrevs =
    ParserNew.inContext "abbrevs" <|
        ParserNew.succeed identity
            |= ParserNew.andThen (\s -> abbrevsHelp [ s ]) parseAbbrev


{-| Check if there is a `nextAbbrev`. If so, continue trying to find
more abbreviations. If not, give back the list accumulated thus far.
-}
abbrevsHelp : List String -> ParserNew.Parser (List String)
abbrevsHelp revTerms =
    oneOf
        [ nextAbbrev
            |> ParserNew.andThen (\s -> abbrevsHelp (s :: revTerms))
        , ParserNew.succeed (List.reverse revTerms)
        ]


nextAbbrev : ParserNew.Parser String
nextAbbrev =
    ParserNew.succeed identity
        |. parseSpace
        |= parseAbbrev


{-| -}
packedTimeZoneTupleNew : ParserNew.Parser ( String, List String, List Float, List Int, List Float )
packedTimeZoneTupleNew =
    ParserNew.succeed (,,,,)
        |= parseName
        |. parseBar
        |= parseAbbrevs
        |. parseBar
        |= parseOffsets
        |. parseBar
        |= parseIndices
        |. parseBar
        |= parseDiffs


parseOffsets : ParserNew.Parser (List Float)
parseOffsets =
    ParserNew.inContext "offsets" <|
        ParserNew.succeed identity
            |= ParserNew.andThen (\f -> offsetsHelp [ f ]) parseOffset


parseOffset : ParserNew.Parser Float
parseOffset =
    (ParserNew.succeed (,,)
        |= parseSign
        |= parseWhole
        |= parseFrac
    )
        |> ParserNew.andThen convertBase60


convertBase60 : ( Int, String, String ) -> ParserNew.Parser Float
convertBase60 ( sign, whole, frac ) =
--    let
--        s1 =
--            Debug.log "sign" sign
--
--        w1 =
--            Debug.log "whole" whole
--
--        f1 =
--            Debug.log "frac" frac
--    in
        if whole == "" && frac == "" then
            ParserNew.fail "expected an alphanumeric character or ."
        else
            ParserNew.succeed <| unsafeBase60 sign whole frac


parseSign : ParserNew.Parser Int
parseSign =
    oneOf
        [ keep (Exactly 1) (\c -> c == '-')
            |> ParserNew.andThen minusOne
        , ParserNew.succeed 1
        ]


minusOne : String -> ParserNew.Parser Int
minusOne hyphen =
    ParserNew.succeed -1


parseWhole : ParserNew.Parser String
parseWhole =
    keep zeroOrMore (\c -> unsafeBase60Digit c)


parseFrac : ParserNew.Parser String
parseFrac =
    oneOf
        [ parseSuccessfulFrac
        , ParserNew.succeed ""
        ]


parseSuccessfulFrac : ParserNew.Parser String
parseSuccessfulFrac =
    ( ParserNew.succeed identity
        |. ignore (Exactly 1) (\c -> c == '.')
        |= keep oneOrMore (\c -> unsafeBase60Digit c)
    )


convertFrac : String -> ParserNew.Parser String
convertFrac frac =
    ParserNew.succeed frac


unsafeBase60Digit : Char -> Bool
unsafeBase60Digit c =
    Char.isDigit c || Char.isUpper c || Char.isLower c


{-| Check if there is a `nextOffset`. If so, continue trying to find
more abbreviations. If not, give back the list accumulated thus far.
Converts the strings to base 60 numbers.
-}
offsetsHelp : List Float -> ParserNew.Parser (List Float)
offsetsHelp revTerms =
    oneOf
        [ nextOffset
            |> ParserNew.andThen (\f -> offsetsHelp (f :: revTerms))
        , ParserNew.succeed (List.reverse revTerms)
        ]


nextOffset : ParserNew.Parser Float
nextOffset =
    ParserNew.succeed identity
        |. parseSpace
        |= parseOffset


parseIndices : ParserNew.Parser (List Int)
parseIndices =
    ParserNew.inContext "indices" <|
        ParserNew.succeed identity
            |= ParserNew.andThen (\i -> indicesHelp [ i ]) parseIndex


{-| Check if there is a `nextIndex`. If so, continue trying to find
more indices. If not, give back the list accumulated thus far.
Each char is converted to an Int.
-}
indicesHelp : List Int -> ParserNew.Parser (List Int)
indicesHelp revTerms =
    oneOf
        [ nextIndex
            |> ParserNew.andThen (\i -> indicesHelp (i :: revTerms))
        , ParserNew.succeed (List.reverse revTerms)
        ]


nextIndex : ParserNew.Parser Int
nextIndex =
    ParserNew.succeed identity
        |= parseIndex


parseIndex : ParserNew.Parser Int
parseIndex =
    keep (Exactly 1) (\c -> Char.isDigit c)
        |> ParserNew.andThen convertDecimal


convertDecimal : String -> ParserNew.Parser Int
convertDecimal digit =
    case String.toInt digit of
        Err msg ->
            ParserNew.fail msg

        Ok value ->
            ParserNew.succeed value


parseDiffs : ParserNew.Parser (List Float)
parseDiffs =
    ParserNew.inContext "diffs" <|
        ParserNew.succeed identity
            |= ParserNew.andThen (\f -> diffsHelp [ f ]) parseDiff


diffsHelp : List Float -> ParserNew.Parser (List Float)
diffsHelp revTerms =
    oneOf
        [ nextDiff
            |> ParserNew.andThen (\f -> diffsHelp (f :: revTerms))
        , ParserNew.succeed (List.reverse revTerms)
        ]


nextDiff : ParserNew.Parser Float
nextDiff =
    ParserNew.succeed identity
        |= parseDiff




parseDiff : ParserNew.Parser Float
parseDiff =
    (ParserNew.succeed (,,)
        |= parseSign
        |= parseWhole
        |= parseFrac
    )
        |> ParserNew.andThen convertBase60Times60000


convertBase60Times60000 : ( Int, String, String ) -> ParserNew.Parser Float
convertBase60Times60000 ( sign, whole, frac ) =
--    let
--        s1 =
--            Debug.log "sign" sign
--
--        w1 =
--            Debug.log "whole" whole
--
--        f1 =
--            Debug.log "frac" frac
--    in
        if whole == "" && frac == "" then
            ParserNew.fail "expected an alphanumeric character or ."
        else
            ParserNew.succeed <| (*) 60000 (unsafeBase60 sign whole frac)


{-| packedTimeZoneNew parses a zone data string into a TimeZone, validating that
the data format invariants hold.
-}
packedTimeZoneNew : ParserNew.Parser TimeZone
packedTimeZoneNew =
    let
        parseOffsets =
            ParserNew.succeed []

        parseIndices =
            ParserNew.succeed []

        parseDiffs =
            ParserNew.succeed []

        convertData : ( String, List String, List Float, List Float, List Float ) -> ParserNew.Parser TimeZone
        convertData ( name, abbrevs, offsets, indices, diffs ) =
            ParserNew.succeed
                (TimeZone
                    { name = "name"
                    , spans = []
                    }
                )

        name =
            Combine.regex "[^|]+"
                <* Combine.string "|"

        abbrevs =
            Combine.sepBy1 (Combine.string " ") (Combine.regex "[^ |]+")
                <* Combine.string "|"

        offsets =
            Combine.sepBy1 (Combine.string " ") base60
                <* Combine.string "|"

        indices =
            (\s -> List.map (\n -> floor <| unsafeBase60 1 n "") (String.split "" s))
                <$> Combine.regex "[^|]+"
                <* Combine.string "|"

        diffs =
            List.map ((*) 60000)
                <$> Combine.sepBy (Combine.string " ") base60

        decode =
            PackedTimeZone
                <$> name
                <*> abbrevs
                <*> offsets
                <*> indices
                <*> diffs

        validate data =
            let
                abbrevs =
                    List.length data.abbrevs

                offsets =
                    List.length data.offsets

                maxIndex =
                    List.maximum data.indices
                        |> Maybe.withDefault 0
            in
                if abbrevs /= offsets then
                    Combine.fail "abbrevs and offsets have different lengths"
                else if maxIndex >= abbrevs then
                    Combine.fail "highest index is longer than both abbrevs and offsets"
                else
                    Combine.succeed data

        span times data i idx =
            { from = times !! i
            , until = times !! (i + 1)
            , abbreviation = data.abbrevs !! idx
            , offset = round (data.offsets !! idx * minuteMs)
            }

        convert data =
            let
                times =
                    if not <| List.isEmpty data.diffs then
                        List.scanl (+) (data.diffs !! 0) (List.drop 1 data.diffs)
                    else
                        []

                -- surround times with - and +infinity
                paddedTimes =
                    [ -1 / 0 ] ++ times ++ [ 1 / 0 ]
            in
                TimeZone
                    { name = data.name
                    , spans = List.indexedMap (span paddedTimes data) data.indices
                    }
    in
        --        convert <$> (decode >>= validate)
        (ParserNew.succeed (,,,,)
            |= parseName
            |. parseBar
            |= parseAbbrevs
            |. parseBar
            |= parseOffsets
            |. parseBar
            |= parseIndices
            |. parseBar
            |= parseDiffs
        )
            |> ParserNew.andThen convertData


base60 : Parser s Float
base60 =
    let
        decode =
            (,,)
                <$> Combine.Num.sign
                <*> Combine.optional "" base60String
                <*> Combine.optional "" (Combine.string "." *> base60String)

        convert ( sign, whole, frac ) =
            if whole == "" && frac == "" then
                Combine.fail "expected an alphanumeric character or ."
            else
                Combine.succeed <| unsafeBase60 sign whole frac
    in
        decode >>= convert


base60String : Parser s String
base60String =
    Combine.regex "[0-9a-zA-Z]+"


unsafeBase60 : Int -> String -> String -> Float
unsafeBase60 sign whole frac =
    let
        toNum c =
            let
                n =
                    Char.toCode c |> toFloat
            in
                if n > 96 then
                    n - 87
                else if n > 64 then
                    n - 29
                else
                    n - 48

        toWhole cs acc =
            case cs of
                [] ->
                    acc

                c :: cs ->
                    toWhole cs (60 * acc + toNum c)

        toFrac cs mul acc =
            let
                mul_ =
                    mul / 60
            in
                case cs of
                    [] ->
                        acc

                    c :: cs ->
                        toFrac cs mul_ (acc + mul_ * toNum c)
    in
        toWhole (String.toList whole) 0
            |> toFrac (String.toList frac) 1
            |> ((*) (toFloat sign))


(!!) : List a -> Int -> a
(!!) xs i =
    case List.head (List.drop i xs) of
        Nothing ->
            Debug.crash ("index too large: xs=" ++ toString xs ++ " i=" ++ toString i)

        Just x ->
            x
