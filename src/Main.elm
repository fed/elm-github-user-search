module Main exposing (main)

import Browser
import Html exposing (..)
import UserProfile


main : Program () UserProfile.Model UserProfile.Msg
main =
    Browser.element
        { init = UserProfile.init
        , update = UserProfile.update
        , subscriptions = \_ -> Sub.none
        , view = UserProfile.view
        }
