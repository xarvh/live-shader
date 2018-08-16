module Main exposing (..)

import App
import Browser


main =
    Browser.sandbox
        { init = App.init
        , update = App.update
        , view = App.view
        }
