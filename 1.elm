import Html.App as Html
import Window
import Mouse
import Task
import Basics.Extra exposing (never)
import AnimationFrame

import View exposing (view)
import Types exposing (..)
import Model exposing (..)
import Utilities exposing (..)


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
    , Mouse.moves MouseMove
    , Mouse.downs RotateR
    , AnimationFrame.diffs Tick
    ]

    
init =
  Model [Just t] Nothing Nothing Nothing 0.0
  => Task.perform never Init Window.size


update : Msg -> Model -> (Model, Cmd a)
update msg model =
  case msg of
    NoOp
    -> model
    => Cmd.none
    
    Init res
    -> ({ model | resolution = Just res })
    => Cmd.none
    
    
    Tick time
    ->
      let piece = Maybe.withDefault Nothing (List.head model.board)
          rotated = rotatePiece <| Maybe.withDefault [(0,0)] piece
      in if model.timeout < 122.34
      then { model | board = [piece], timeout = model.timeout+time } => Cmd.none
      else { model | board = [Just rotated], timeout = 0.0 } => Cmd.none
        
    MouseMove pos
    -> ({ model | mouse = Just pos})
    => Cmd.none
    
    Resize newRes
    -> ({ model | resolution = Just newRes })
    => Cmd.none
    
    RotateR pos
    ->
      let piece = Maybe.withDefault Nothing (List.head model.board)
          rotated = rotatePiece <| Maybe.withDefault [(0,0)] piece
      in { model | board = [Just rotated] }
    => Cmd.none
    
    RotateL block
    -> model
    => Cmd.none
