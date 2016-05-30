import Html.App as Html
import Window
import Keyboard
import Task exposing (succeed)
import AnimationFrame
import Basics.Extra exposing (never)

import View exposing (view)
import Types exposing (..)
import Model exposing (..)
import Utilities exposing (..)
import Update exposing (..)

main =
  Html.program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Resize 
    -- , Mouse.moves MouseMove
    , AnimationFrame.diffs Tick
    , Keyboard.downs KeyDown
    ]


init =
  setActivePiece emptyModel (0,6) i
  => Task.perform never Init Window.size