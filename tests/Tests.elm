module Tests exposing (..)

import Test exposing (..)
import TestCalendar
import TestDateTime


all : Test
all =
    describe "elm-utc"
        [ TestCalendar.all
        , TestDateTime.all
        ]
