module Main exposing (main)

{-| An Elm web client application to exercise
the `elm-time` library.


# Main entry

@docs main

-}

import Html exposing (Html, pre, text)
import Time.DateTime exposing (fromISO8601)
import Time.ISO8601Error exposing (renderText, reflow)


{-| The "entry"
-}
main : Html msg
main =
    case fromISO8601 "1991-02-29T12:25:12.0Z" of
        Ok v ->
            text <| toString v

        Err e ->
            let
                msg =
                    renderText e
                        ++ "\n\n"
                        ++ reflow (toString e)
            in
            Html.pre [] [ text <| msg ]
