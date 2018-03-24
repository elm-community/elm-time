module Time.ISO8601Error exposing
    ( renderText
    )

{-| A renderer to format error messages resulting from
ISO8601 parsing errors.

At this time, there is a renderer, **renderText**, to render
a fixed-font message to, say, a terminal screen.


# Main entry

@docs renderText

# Configuration

@docs primitives

-}

import Char
import Parser exposing (Parser)
import Set exposing (Set)


{-|
-}
primitives : Set String
primitives =
    Set.fromList ["date", "time", "datetime"]


{-| Invoking the renderer.  This returns an error string

-}
renderText : Parser.Error -> String
renderText error =
    let
        findContext : List Parser.Context -> String
        findContext contexts =
            List.foldl
                (\ctx acc ->
                    if acc == Nothing then
                        if Set.member ctx.description primitives then
                            Nothing
                        else
                            Just <| forContext ctx
                    else
                        acc
                )
                Nothing
                contexts
                |> Maybe.withDefault noContext

        ( cause, context ) =
            case error.context of
                [] ->
                    ( Nothing, noContext )

                x :: xs ->
                    if Set.member x.description primitives then
                        ( Just x.description, findContext xs )
                    else
                        ( Nothing, forContext x )
    in
    context
        ++ "\n\n    "
        ++ relevantSource error
        ++ "\n    "
        ++ marker error.col
        ++ "\n\n"
        ++ (reflow <| describeProblem cause error.problem)


reflow : String -> String
reflow s =
    let
        flowLine : String -> String
        flowLine s =
            String.words s
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

                                Just s ->
                                    s ++ " " ++ word
                    in
                    if String.length combined > 72 then
                        ( Just word, sentence :: acc )
                    else
                        ( Just combined, acc )
                )
                ( Nothing, [] )
                words
                |> uncurry (::)
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


relevantSource : Parser.Error -> String
relevantSource { row, source } =
    String.lines source
        |> List.drop (row - 1)
        |> List.head
        |> Maybe.withDefault ""


describeProblem : Maybe String -> Parser.Problem -> String
describeProblem probableCause problem =
    case problem of
        Parser.BadInt ->
            "Unable to read an integer here."

        Parser.BadFloat ->
            "Unable to read a float here"

        Parser.BadRepeat ->
            case probableCause of
                Just cause ->
                    "Can't fine a " ++ cause ++ "."

                Nothing ->
                    "I got stuck here. I'm probably looking for something specific and not making any progress here."

        Parser.ExpectingEnd ->
            "String should have stopped here, but it goes on."

        Parser.ExpectingSymbol s ->
            "Expecting a `" ++ s ++ "` here."

        Parser.ExpectingKeyword s ->
            "Expecting a keyword `" ++ s ++ "` here."

        Parser.ExpectingVariable ->
            "Expecting a variable here."

        Parser.ExpectingClosing s ->
            "Expecting a closing `" ++ s ++ "` here."

        Parser.Fail s ->
            s

        Parser.BadOneOf problems ->
            "Encountering multiple problems:\n\n"
                ++ (List.map (describeProblem probableCause) problems |> String.join "\n\n")


marker : Int -> String
marker col =
    String.repeat (col - 1) " " ++ "^"


forContext : Parser.Context -> String
forContext { description } =
    "Failed to parse the '" ++ description ++ "' segment:"


noContext : String
noContext =
    "I ran into a problem parsing this:"
