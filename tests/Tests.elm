module Tests exposing (..)

import Test exposing (..)
import TestDate
import TestDateTime


all : Test
all =
    describe "elm-utc"
        [ TestDate.all
        , TestDateTime.all
        ]
