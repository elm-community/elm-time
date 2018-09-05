module Time.Iso8601ErrorMsg exposing
    ( renderText
    , reflow
    )

{-| A renderer to format error messages resulting from
ISO8601 parsing errors.

At this time, there is a renderer, **renderText**, to render
a fixed-font message to, say, a terminal screen. This renderer
strives to output the friendly useful error message that elm
is famous for.


# Main entry

@docs renderText


# Utilities

@docs reflow

-}

import Parser.Advanced exposing (DeadEnd)
import Time.Iso8601 exposing (Problem(..))


{-| Invoking the renderer. This returns an 'elm compiler-style formatted' error string

    import Parser.Advanced exposing (DeadEnd)
    import Time.Iso8601 exposing (Problem)

    -- The \n in the middle of the string is to provide for the 72-char right margin
    failString : String
    failString =
        "1991-02-29 is not a valid date"

    renderedString : String
    renderedString =
        "The 'day-of-month' segment is invalid:\n\n" ++
        "    1991-02-29T12:25:12.0Z\n" ++
        "            ^\n\n" ++
        failString

    showError : String -> String
    showError input =
       case Time.Iso8601.toDateTime input of
           Ok _ ->
               "success?! what?!"
           Err errors ->
               String.join "\n" <| List.map (renderText input) errors


    showError "1991-02-29T12:25:12.0Z"
    --> renderedString

-}
renderText : String -> DeadEnd String Problem -> String
renderText source error =
    let
        -- Hack: special handling of the leap year.
        -- This sets the marker under the beginning of the
        -- day-of-month segment.
        tweakCol : ParserContext -> Int
        tweakCol ctx =
            if ctx.context == "leap-year" then
                ctx.col - 2

            else
                ctx.col

        ( diagnosis, col ) =
            case List.head error.contextStack of
                Nothing ->
                    ( noContext, error.col )

                Just ctx ->
                    ( forContext ctx error.problem
                    , tweakCol ctx
                    )
    in
    diagnosis
        ++ "\n\n    "
        ++ relevantSource source error
        ++ "\n    "
        ++ marker col
        ++ "\n\n"
        ++ (reflow <| describeProblem error.problem)


{-| A convenience function to auto-wrap long strings

Use this method to appropriately wrap the error string returned
from `renderText` above.

    -- \n inserted at 72nt position coz is right margin.
    reflow "Expecting the value 29 to be in the range 1 to 28 for the specified year, 1991, and month, 2."
    --> "Expecting the value 29 to be in the range 1 to 28 for the specified" ++ "\n" ++ "year, 1991, and month, 2."

-}
reflow : String -> String
reflow s =
    let
        flowLine : String -> String
        flowLine str =
            String.words str
                |> makeSentences
                |> String.join "\n"

        makeSentences : List String -> List String
        makeSentences words =
            List.foldl
                (\word ( sentence, acc ) ->
                    let
                        combined =
                            case sentence of
                                Nothing ->
                                    word

                                Just str ->
                                    str ++ " " ++ word
                    in
                    if String.length combined > 72 then
                        ( Just word, sentence :: acc )

                    else
                        ( Just combined, acc )
                )
                ( Nothing, [] )
                words
                |> (\( a, b ) -> (::) a b)
                |> reverseFilterMap identity
    in
    s
        |> String.lines
        |> List.map flowLine
        |> String.join "\n"


reverseFilterMap : (a -> Maybe b) -> List a -> List b
reverseFilterMap toMaybe list =
    List.foldl
        (\x acc ->
            case toMaybe x of
                Just y ->
                    y :: acc

                Nothing ->
                    acc
        )
        []
        list


relevantSource : String -> DeadEnd String Problem -> String
relevantSource source { row } =
    String.lines source
        |> List.drop (row - 1)
        |> List.head
        |> Maybe.withDefault ""


describeProblem : Problem -> String
describeProblem problem =
    case problem of
        ExpectingDigit ->
            "Expecting a digit here"

        ExpectingRange value low high ->
            "Expecting "
                ++ String.fromInt value
                ++ " to be between "
                ++ String.fromInt low
                ++ " and "
                ++ String.fromInt high

        ExpectingDot ->
            "Expecting a '.' here"

        ExpectingZ ->
            "Expecting a Z here"

        ExpectingSign ->
            "Expecting a sign (+ or -) here"

        BadInt ->
            "Unable to parse a valid integer"

        InvalidDate ( year, month, day ) ->
            let
                pad : Int -> Int -> String
                pad amount =
                    String.fromInt >> String.padLeft amount '0'

                dateStr =
                    String.join "-" [ pad 4 year, pad 2 month, pad 2 day ]
            in
            dateStr ++ " is not a valid date"

        ExpectingNDigits count string ->
            "Expecting to get " ++ String.fromInt count ++ " digits here, not " ++ string

        Other str ->
            str


adjustMarker : DeadEnd String Problem -> ParserContext -> Int
adjustMarker error context =
    case error.problem of
        Other msg ->
            context.col

        _ ->
            error.col


marker : Int -> String
marker col =
    String.repeat (col - 1) " " ++ "^"


type alias ParserContext =
    { row : Int, col : Int, context : String }


forContext : ParserContext -> Problem -> String
forContext { context } problem =
    let
        -- hack; can't find any better way to do this.
        segment =
            if context == "leap-year" then
                "day-of-month"

            else
                context
    in
    case problem of
        InvalidDate _ ->
            "The '" ++ segment ++ "' segment is invalid:"

        ExpectingRange _ _ _ ->
            "The '" ++ segment ++ "' segment is invalid:"

        ExpectingNDigits _ _ ->
            "The '" ++ segment ++ "' segment is invalid:"

        _ ->
            "Failed to parse the '" ++ segment ++ "' segment:"


noContext : String
noContext =
    "I ran into a problem parsing this:"
