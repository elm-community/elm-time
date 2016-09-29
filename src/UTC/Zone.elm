module UTC.Zone
    exposing
        ( Zone
        , Span
        , abbreviation
        , offset
        , unpack
        )

{-| FIXME

# Zone values
@docs Zone, Span, abbreviation, offset

# Constructing Zones
@docs unpack
-}

import Char
import Combine exposing (Parser)
import Combine.Infix exposing ((<$>), (<*>), (<*), (*>))
import Combine.Num
import String
import Time exposing (Time)


{-| Zone represents the type of timezone values.  These are generally
loaded from an external source via `unpack`.

See also http://momentjs.com/timezone/docs/#/data-formats/packed-format/.
-}
type alias Zone =
    { name : String
    , spans : List Span
    }


{-| -}
type alias Span =
    { start : Time
    , end : Time
    , abbreviation : String
    , offset : Float
    }


{-| Given an arbitrary Time and a Zone, abbreviation returns Just
the Zone's abbreviation at that Time or Nothing.
-}
abbreviation : Time -> Zone -> Maybe String
abbreviation time { spans } =
    find time spans
        |> Maybe.map .abbreviation


{-| Given an arbitrary Time and a Zone, offset returns Just the
Zone's UTC offset in minutes at that Time or Nothing.
-}
offset : Time -> Zone -> Maybe Float
offset time { spans } =
    find time spans
        |> Maybe.map .offset


find : Time -> List Span -> Maybe Span
find time spans =
    let
        go xs =
            case xs of
                [] ->
                    Nothing

                x :: xs ->
                    if time >= x.start && time < x.end then
                        Just x
                    else
                        go xs
    in
        go spans


{-| unpack decodes a packed zone data object into a Zone value.

See also http://momentjs.com/timezone/docs/#/data-formats/packed-format/
-}
unpack : String -> Result (List String) Zone
unpack data =
    case Combine.parse packedZone data of
        ( Err errors, _ ) ->
            Err errors

        ( Ok zone, _ ) ->
            Ok zone


type alias PackedZone =
    { name : String
    , abbrevs : List String
    , offsets : List Float
    , indices : List Int
    , diffs : List Float
    }


{-| packedZone parses a zone data string into a Zone, validating that
the data fromat invariants hold.
-}
packedZone : Parser Zone
packedZone =
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
            PackedZone
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
                    Combine.fail [ "abbrevs and offsets have different lengths" ]
                else if maxIndex >= abbrevs then
                    Combine.fail [ "highest index is longer than both abbrevs and offsets" ]
                else
                    Combine.succeed data

        span times data i idx =
            { start = times !! i
            , end = times !! (i + 1)
            , abbreviation = data.abbrevs !! idx
            , offset = data.offsets !! idx
            }

        convert data =
            let
                times =
                    if not <| List.isEmpty data.diffs then
                        List.scanl (+) (data.diffs !! 0) (List.drop 1 data.diffs)
                    else
                        []

                -- surround times with - and +infinity
                times' =
                    [ -1 / 0 ] ++ times ++ [ 1 / 0 ]
            in
                { name = data.name
                , spans = List.indexedMap (span times' data) data.indices
                }
    in
        convert <$> (decode `Combine.andThen` validate)


base60 : Parser Float
base60 =
    let
        decode =
            (,,)
                <$> Combine.Num.sign
                <*> Combine.optional "" base60String
                <*> Combine.optional "" (Combine.string "." *> base60String)

        convert ( sign, whole, frac ) =
            if whole == "" && frac == "" then
                Combine.fail [ "expected an alphanumeric character or ." ]
            else
                Combine.succeed <| unsafeBase60 sign whole frac
    in
        decode `Combine.andThen` convert


base60String : Parser String
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
                mul' =
                    mul / 60
            in
                case cs of
                    [] ->
                        acc

                    c :: cs ->
                        toFrac cs mul' (acc + mul' * toNum c)
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
