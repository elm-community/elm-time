module Time.TimeZone exposing
    ( TimeZone, name, abbreviation, offset, offsetString
    , setName
    , unpack
    , errorZone, find, parseAbbrevs, parseDiffs, parseIndices, parseName, parseOffsets, pipe
    )

{-| This module defines a representations for Timezone information.


# TimeZone values

@docs TimeZone, name, abbreviation, offset, offsetString


# Manipulating TimeZones

@docs setName


# Constructing TimeZones

@docs unpack

-}

import Array
import Char
import List.Extra
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , andThen
        , chompIf
        , chompWhile
        , end
        , getChompedString
        , map
        , oneOf
        , problem
        , run
        , succeed
        )
import Time exposing (Posix)
import Time.Internal exposing (..)


{-| TimeZone represents the opaque type of timezone values. These are
generally loaded from an external source via `unpack`.

See also <http://momentjs.com/timezone/docs/#/data-formats/packed-format/>.

-}
type TimeZone
    = TimeZone
        { name : String
        , spans : SpanList
        }


type SpanList
    = Default Span
    | More Span SpanList


errorZone : String -> TimeZone
errorZone errors =
    TimeZone { name = "error: " ++ errors, spans = Default { until = 0, abbreviation = "error", offset = 0 } }


{-| Spans represent variations within a TimeZone. A Time has an
associated Span if it is has the least .until, such that `t < .until`.

`offset` is the Span's UTC offset in milliseconds.

-}
type alias Span =
    { until : Float
    , abbreviation : String
    , offset : Int
    }


{-| Given an arbitrary Time and a TimeZone, abbreviation returns the
TimeZone's abbreviation at that Time.
-}
abbreviation : Posix -> TimeZone -> String
abbreviation time (TimeZone { spans }) =
    find time spans |> .abbreviation


{-| Given an arbitrary Time and a TimeZone, offset returns the
TimeZone's UTC offset in milliseconds at that Time.
-}
offset : Posix -> TimeZone -> Int
offset time (TimeZone { spans }) =
    find time spans |> .offset


{-| Given an arbitrary Time and TimeZone, offsetString returns an
ISO8601-formatted UTC offset for at that Time.
-}
offsetString : Posix -> TimeZone -> String
offsetString time timeZone =
    let
        utcOffset =
            offset time timeZone // minuteMs

        hours =
            abs utcOffset // 60

        minutes =
            modBy 60 (abs utcOffset)

        string =
            padded hours ++ ":" ++ padded minutes
    in
    if utcOffset <= 0 then
        "+" ++ string

    else
        "-" ++ string


find : Posix -> SpanList -> Span
find time spans =
    let
        ms =
            Time.posixToMillis time |> toFloat

        go : Float -> SpanList -> Span
        go prev xs =
            case xs of
                Default span ->
                    span

                More span other ->
                    if prev <= ms && ms < span.until then
                        span

                    else
                        go span.until other
    in
    go (-1 / 0) spans


{-| setName updates a TimeZone's name.
-}
setName : String -> TimeZone -> TimeZone
setName newName (TimeZone tz) =
    TimeZone { tz | name = newName }


{-| name returns a TimeZone's name.
-}
name : TimeZone -> String
name (TimeZone tz) =
    tz.name


{-| unpackNew decodes a packed zone data object into a TimeZone value.

See also <http://momentjs.com/timezone/docs/#/data-formats/packed-format/>

-}
unpack : String -> Result (List DeadEnd) TimeZone
unpack data =
    run packedTimeZone data


{-| packedTimeZoneNew parses a zone data string into a TimeZone, validating that
the data format invariants hold.
-}
packedTimeZone : Parser TimeZone
packedTimeZone =
    let
        decode : Parser PackedTimeZone
        decode =
            succeed PackedTimeZone
                |= parseName
                |. pipe
                |= parseAbbrevs
                |. pipe
                |= parseOffsets
                |. pipe
                |= parseIndices
                |. pipe
                |= parseDiffs

        unpack_ : PackedTimeZone -> Parser UnpackedTimeZone
        unpack_ data =
            let
                -- Only the first timestamp is absolute; the rest are relative offsets.
                untils =
                    data.diffs |> List.Extra.scanl1 (+)

                -- The packed format implicitly has an additional span at the end which continues
                -- indefinitely, so we add +Infinity to the list
                paddedUntils =
                    untils ++ [ 1 / 0 ]

                abbrArray =
                    Array.fromList data.abbrevs

                abbrevs =
                    List.filterMap (\index -> Array.get index abbrArray) data.indices

                offsetsArray =
                    data.offsets
                        |> List.map round
                        |> List.map ((*) minuteMs)
                        |> Array.fromList

                offsets =
                    List.filterMap (\index -> Array.get index offsetsArray) data.indices
            in
            if List.length abbrevs /= List.length paddedUntils then
                problem <|
                    "abbrevs was of length "
                        ++ (String.fromInt <| List.length abbrevs)
                        ++ "expected "
                        ++ (String.fromInt <| List.length paddedUntils)

            else if List.length offsets /= List.length paddedUntils then
                problem <|
                    "offsets was of length "
                        ++ (String.fromInt <| List.length offsets)
                        ++ "expected "
                        ++ (String.fromInt <| List.length paddedUntils)

            else
                succeed
                    { name = data.name
                    , spans = List.map3 Span paddedUntils abbrevs offsets
                    }

        convert : UnpackedTimeZone -> Parser TimeZone
        convert unpacked =
            case List.reverse unpacked.spans of
                [] ->
                    problem "no spans"

                default :: others ->
                    succeed <|
                        TimeZone
                            { name = unpacked.name
                            , spans = List.foldl More (Default default) others
                            }
    in
    decode
        |> andThen unpack_
        |> andThen convert


type alias UnpackedTimeZone =
    { name : String
    , spans : List Span
    }


type alias PackedTimeZone =
    { name : String
    , abbrevs : List String
    , offsets : List Float
    , indices : List Int
    , diffs : List Float
    }


{-| Parse the name of the timezone
-}
parseName : Parser String
parseName =
    getChompedString <|
        succeed identity
            |. chompWhile ((/=) '|')


{-| Parse the first abbrev and then use `abbrevsHelp` to find
the remaining ones.
-}
parseAbbrevs : Parser (List String)
parseAbbrevs =
    let
        abbrev : Parser String
        abbrev =
            getChompedString <|
                chompWhile (\c -> c /= ' ' && c /= '|')

        helper : List String -> Parser (List String)
        helper revTerms =
            oneOf
                [ next
                    |> andThen (\s -> helper (s :: revTerms))
                , succeed (List.reverse revTerms)
                ]

        next : Parser String
        next =
            succeed identity
                |. parseSpace
                |= abbrev
    in
    succeed identity
        |= andThen (\s -> helper [ s ]) abbrev


parseOffsets : Parser (List Float)
parseOffsets =
    let
        offset_ : Parser Float
        offset_ =
            (succeed (\a b c -> ( a, b, c ))
                |= parseSign
                |= parseWhole
                |= parseFrac
            )
                |> andThen convertBase60

        convertBase60 : ( Int, String, String ) -> Parser Float
        convertBase60 ( sign, whole, frac ) =
            if whole == "" && frac == "" then
                problem "expected an alphanumeric character or ."

            else
                succeed <| unsafeBase60 sign whole frac

        convertFrac : String -> Parser String
        convertFrac frac =
            succeed frac

        helper : List Float -> Parser (List Float)
        helper revTerms =
            oneOf
                [ next
                    |> andThen (\f -> helper (f :: revTerms))
                , succeed (List.reverse revTerms)
                ]

        next : Parser Float
        next =
            succeed identity
                |. parseSpace
                |= offset_
    in
    succeed identity
        |= andThen (\f -> helper [ f ]) offset_


parseIndices : Parser (List Int)
parseIndices =
    let
        helper : List Int -> Parser (List Int)
        helper revTerms =
            oneOf
                [ next
                    |> andThen (\i -> helper (i :: revTerms))
                , succeed (List.reverse revTerms)
                ]

        next : Parser Int
        next =
            succeed identity
                |= index

        index : Parser Int
        index =
            getChompedString (chompIf (\c -> Char.isDigit c))
                |> andThen convertDecimal

        convertDecimal : String -> Parser Int
        convertDecimal digit =
            case String.toInt digit of
                Nothing ->
                    problem <| "failed to parse int from " ++ digit

                Just value ->
                    succeed value
    in
    succeed identity
        |= andThen (\i -> helper [ i ]) index


parseDiffs : Parser (List Float)
parseDiffs =
    let
        emptyDiffs : Parser (List Float)
        emptyDiffs =
            (succeed identity
                |. pipe
            )
                |> andThen (\_ -> succeed [])

        diffsEnd : Parser (List Float)
        diffsEnd =
            (succeed identity
                |. end
            )
                |> andThen (\_ -> succeed [])

        helper : List Float -> Parser (List Float)
        helper revTerms =
            oneOf
                [ next
                    |> andThen (\f -> helper (f :: revTerms))
                , succeed (List.reverse revTerms)
                ]

        next : Parser Float
        next =
            succeed identity
                |. parseSpace
                |= diff

        diff : Parser Float
        diff =
            (succeed (\a b c -> ( a, b, c ))
                |= parseSign
                |= parseWhole
                |= parseFrac
            )
                |> andThen convertBase60Times60000

        convertBase60Times60000 : ( Int, String, String ) -> Parser Float
        convertBase60Times60000 ( sign, whole, frac ) =
            if whole == "" && frac == "" then
                problem "expected an alphanumeric character or ."

            else
                succeed <| (*) 60000 (unsafeBase60 sign whole frac)
    in
    oneOf
        [ emptyDiffs
        , diffsEnd
        , andThen (\f -> helper [ f ]) diff
        ]


pipe : Parser ()
pipe =
    chompIf ((==) '|')


parseSpace : Parser ()
parseSpace =
    chompIf ((==) ' ')


parseSign : Parser Int
parseSign =
    let
        minusOne : String -> Parser Int
        minusOne hyphen =
            if hyphen == "-" then
                succeed -1

            else
                problem "failed to chomp minus"
    in
    oneOf
        [ getChompedString (chompIf (\c -> c == '-')) |> andThen minusOne
        , succeed 1
        ]


parseWhole : Parser String
parseWhole =
    getChompedString <|
        chompWhile (\c -> unsafeBase60Digit c)


parseFrac : Parser String
parseFrac =
    oneOf
        [ parseSuccessfulFrac
        , succeed ""
        ]


unsafeBase60Digit : Char -> Bool
unsafeBase60Digit c =
    Char.isDigit c || Char.isUpper c || Char.isLower c


parseSuccessfulFrac : Parser String
parseSuccessfulFrac =
    getChompedString <|
        succeed identity
            |. chompIf (\c -> c == '.')
            |. chompWhile (\c -> unsafeBase60Digit c)


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

                c :: cs_ ->
                    toWhole cs_ (60 * acc + toNum c)

        toFrac cs mul acc =
            let
                mul_ =
                    mul / 60
            in
            case cs of
                [] ->
                    acc

                c :: cs_ ->
                    toFrac cs_ mul_ (acc + mul_ * toNum c)
    in
    toWhole (String.toList whole) 0
        |> toFrac (String.toList frac) 1
        |> (*) (toFloat sign)
