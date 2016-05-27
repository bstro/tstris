import Html.App as Html
import Window
import Mouse
import Task exposing (succeed)
import Basics.Extra exposing (never)
import List.Extra exposing (getAt)
import AnimationFrame
import Random

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
    , AnimationFrame.diffs Tick
    ]

    
init =
  setActivePiece emptyModel (0,0) j
  => Task.perform never Init Window.size


setActivePiece : Model -> Position -> Tetrimino -> Model
setActivePiece model p t =
  let
    board = model.board
    activeBlock = board.activeBlock
    newBoard = { board | activeBlock = Just (p, t) }
  in
    { model | board = newBoard }
  

getPiece : Int -> Tetrimino
getPiece x = Maybe.withDefault i (getAt x tetriminos)


gravity : Position -> Position
gravity (x, y) =
  let foo = Debug.log "foo" (y) in
  (x, y+1)
  -- if y < h then (x, 0)
  -- else 

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({level} as model) =
  case msg of
    NoOp
    -> model
    => Cmd.none
    
    Init res
    -> ({ model | resolution = Just res })
    => Cmd.none
    
    Tick time ->
    let
      t = round time
    in
      if model.timeout < model.level
      then { model | timeout = model.timeout+t } => Cmd.none
      else { model | timeout = 0 } =>
        case model.board.activeBlock of
          Just b -> Task.perform never Step (succeed b)
          Nothing -> Cmd.none

    Step (p, t)
    ->
      let 
        c = gravity p
        next = setActivePiece model c t
        new = setActivePiece model
        {board} = model
        {positions, activeBlock} = board
        -- newPositions = 
      in
      
      case next.board.activeBlock of
        Just ((x,y), t) ->
          if y < h then next => Cmd.none 
          else model => Cmd.none
          -- else ADD CURRENT PIECE TO POSITIONS => THEN GET NEW PIECE
          
        Nothing -> model => Cmd.none
      

    Rotate (p, t)
    -> setActivePiece model p (rotatePiece t)
    => Cmd.none
    
    NextLevel
    -> ({ model | level = level-100 })
    => Cmd.none
    
    MouseMove pos
    -> ({ model | mouse = Just pos})
    => Cmd.none
    
    Resize newRes
    -> ({ model | resolution = Just newRes })
    => Cmd.none

    RandomPiece
    -> model
    => (Random.generate NewPiece (Random.int 0 6))

    NewPiece r
    -> (setActivePiece model (0,0) (getPiece r))
    => Cmd.none