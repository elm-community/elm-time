module Time.TimeZone
    exposing
        ( TimeZone
        , name
        , abbreviation
        , offset
        , offsetString
        , setName
        , unpack
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
import Combine exposing (..)
import Combine.Num
import Time exposing (Time)
import Time.Internal exposing (..)


{-| TimeZone represents the opaque type of timezone values.  These are
generally loaded from an external source via `unpack`.

See also http://momentjs.com/timezone/docs/#/data-formats/packed-format/.
-}
type TimeZone
    = TimeZone
        { name : String
        , spans : List Span
        }


{-| Spans represent variations within a TimeZone.  A Time has an
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

See also http://momentjs.com/timezone/docs/#/data-formats/packed-format/
-}
unpack : String -> Result (List String) TimeZone
unpack data =
    case Combine.parse packedTimeZone data of
        Ok ( _, _, zone ) ->
            Ok zone

        Err ( _, _, errors ) ->
            Err errors


type alias PackedTimeZone =
    { name : String
    , abbrevs : List String
    , offsets : List Float
    , indices : List Int
    , diffs : List Float
    }


{-| packedTimeZone parses a zone data string into a TimeZone, validating that
the data fromat invariants hold.
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
