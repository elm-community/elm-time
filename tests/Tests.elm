module Tests exposing (..)

import Test exposing (..)
import TestDate
import TestDateTime
import TestZonedDateTime


all : Test
all =
    describe "elm-utc"
        [ TestDate.all
        , TestDateTime.all
        , TestZonedDateTime.all
        ]
