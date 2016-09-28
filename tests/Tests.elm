module Tests exposing (..)

import Test exposing (..)
import TestCalendar exposing (all)


all : Test
all =
    describe "elm-utc"
        [ TestCalendar.all
        ]
