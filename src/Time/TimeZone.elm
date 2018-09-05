module Time.TimeZone exposing
    ( TimeZone, name, abbreviation, offset, offsetString
    , setName
    , unpack
    , errorZone
    )

{-| This module defines a representations for Timezone information.


# TimeZone values

@docs TimeZone, name, abbreviation, offset, offsetString


# Manipulating TimeZones

@docs setName


# Constructing TimeZones

@docs unpack

-}

import Char
import Debug
    exposing
        ( log
        )
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
        , spans : List Span
        }


errorZone : String -> TimeZone
errorZone errors =
    TimeZone { name = "error: " ++ errors, spans = [] }


{-| Spans represent variations within a TimeZone. A Time has an
associated Span if `.from <= t < .until`.

`offset` is the Span's UTC offset in milliseconds.

-}
type alias Span =
    { from : Posix
    , until : Posix
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


find : Posix -> List Span -> Span
find time spans =
    let
        go xs =
            case xs of
                [] ->
                    Debug.todo "find: invalid span list"

                x :: ys ->
                    if Time.posixToMillis time >= Time.posixToMillis x.from && Time.posixToMillis time < Time.posixToMillis x.until then
                        x

                    else
                        go ys
    in
    go spans


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

        validate : PackedTimeZone -> Parser PackedTimeZone
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
                problem "abbrevs and offsets have different lengths"

            else if maxIndex >= abbrevs then
                problem "highest index is longer than both abbrevs and offsets"

            else
                succeed data

        span : List Float -> PackedTimeZone -> Int -> Int -> Span
        span times data i idx =
            { from = Time.millisToPosix <| round <| getIndex times i
            , until = Time.millisToPosix <| round <| getIndex times (i + 1)
            , abbreviation = getIndex data.abbrevs idx
            , offset = round (getIndex data.offsets idx * minuteMs)
            }

        convert : PackedTimeZone -> TimeZone
        convert data =
            let
                times =
                    if not <| List.isEmpty data.diffs then
                        List.Extra.scanl (+) (getIndex data.diffs 0) (List.drop 1 data.diffs)

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
    decode
        |> andThen validate
        |> map convert


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


getIndex : List a -> Int -> a
getIndex xs i =
    case List.head (List.drop i xs) of
        Nothing ->
            Debug.todo ("index too large: xs=" ++ Debug.toString xs ++ " i=" ++ String.fromInt i)

        Just x ->
            x
